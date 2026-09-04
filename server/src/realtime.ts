import _ from "lodash";

/* ==== TYPES ==== */

export type User =
  { uuid: string
  , userId: string
  , ws: WebSocket
  , m: Mode
  };

interface ViewingMode { kind: 'viewing', cardId: string };
interface EditingMode { kind: 'editing', cardId: string };

type Mode = ViewingMode | EditingMode;

type ChannelMap = Map<string, User[]>;

type MessageData =
  {
    uid: string,
    tr: string
    u: string,
    m: ['a', string] | ['e', string] | ['d', string]
  };




/* ==== PRIVATE FUNCTIONS ==== */

function disconnectUserByWS(channels : ChannelMap, ws : WebSocket) {
  for (const [treeId, users] of channels) {
    channels.set(treeId, users.filter(u => u.ws !== ws));
  }
  deleteEmptyTreeChannels(channels);
}

function broadcastToChannel(channels : ChannelMap, treeId : string, senderUuid : string, message : any) {
  const users = channels.get(treeId);
  if (users) {
    for (const u of users) {
      if (u.uuid !== senderUuid) {
        u.ws.send(JSON.stringify(message));
      }
    }
  }
}

function broadcastToChannelByWS(channels: ChannelMap, ws: WebSocket, senderUuid: string, message: any) {
  let channelToBroadcast: string | null = null;

  for (const [treeId, users] of channels) {
    if (users.some(u => u.ws === ws)) {
      channelToBroadcast = treeId;
      break;
    }
  }

  if (channelToBroadcast) {
    broadcastToChannel(channels, channelToBroadcast, senderUuid, message);
  }
}

function deleteEmptyTreeChannels(channels : ChannelMap) {
  for (const [treeId, users] of channels) {
    if (users.length === 0) {
      channels.delete(treeId);
    }
  }
}

function updateMode(channels : ChannelMap, uuid : string, mode : ['a', string] | ['e', string] | ['d', string]) {
  for (const [treeId, users] of channels) {
    const user = users.find(u => u.uuid === uuid);
    if (user) {
      switch (mode[0]) {
        case 'a':
          user.m = { kind: 'viewing', cardId: mode[1] };
          break;
        case 'e':
          user.m = { kind: 'editing', cardId: mode[1] };
          break;
        case 'd':
          user.m = { kind: 'viewing', cardId: '' };
          break;
      }
    }
  }
}




/* ==== PUBLIC FUNCTIONS ==== */

export function handleRT(channels : ChannelMap, userId: string, msgData : MessageData) {
  updateMode(channels, msgData.uid, msgData.m);
  broadcastToChannel(channels, msgData.tr, msgData.uid,
    {
      t: 'rt',
      d: _.omit({...msgData, u: userId}, 'tr')
    });
}

export function join(channels : ChannelMap, userId: string, ws: WebSocket, msgData : MessageData) {
  const { uid: uuid, tr: treeId, m } = msgData;
  const channelUsers = channels.get(treeId) || [];
  const user: User = { uuid, userId, ws, m: messageToMode(m) };

  // Add user to channel
  if (channelUsers.length === 0) {
    channels.set(treeId, [user]);
  } else {
    channels.set(treeId, _.uniqBy([...channelUsers, user], 'uuid'));
  }

  // Remove user from other channels
  for (const [otherTreeId, otherUsers] of channels) {
    if (otherTreeId !== treeId && otherUsers.some(u => u.uuid === uuid)) {
      channels.set(otherTreeId, otherUsers.filter(u => u.uuid !== uuid));

      broadcastToChannel(channels, otherTreeId, uuid, {
        t: 'rt',
        d: {uid: uuid, u: userId, m: ["d", ""]}
      })
    }
  }

  // Respond with other users in channel
  const otherUsersInChannel = channelUsers.filter(u => u.uuid !== user.uuid);
  ws.send(JSON.stringify({
    t: 'rt:users',
    d: otherUsersInChannel.map(u => ({uid: u.uuid, u: u.userId, m: modeToMessage(u.m)}))
  }));

}


export function disconnectWebSocket(channels : ChannelMap, ws: WebSocket) {
  const user = [...channels.values()].flatMap(x => x).find(ci => ci.ws === ws);
  if (!user) { return; }

  broadcastToChannelByWS(channels, ws, user.uuid,
    {
      t: 'rt',
      d: {
        u: user.userId, uid: user.uuid, m: ["d", ""]
      }
    });
  disconnectUserByWS(channels, ws);
}




/* ==== UTILITY FUNCTIONS ==== */

function modeToMessage(m : Mode) {
  switch (m.kind) {
    case 'viewing':
      return ['a', m.cardId];
    case 'editing':
      return ['e', m.cardId];
  }
}

function messageToMode(m : ['a', string] | ['e', string] | ['d', string] | null) : Mode {
  if (!m) { return { kind: 'viewing', cardId: '' }; }

  switch (m[0]) {
    case 'a':
      return { kind: 'viewing', cardId: m[1] };
    case 'e':
      return { kind: 'editing', cardId: m[1] };
    case 'd':
      return { kind: 'viewing', cardId: '' };
  }
}


type UserWithStringifiedM = {uuid: string, userId: string, m: string};

function stringifyChannels(channels: ChannelMap): Map<string, UserWithStringifiedM[]> {
  // Create a new Map to store the results
  const result = new Map<string, UserWithStringifiedM[]>();

  // Iterate over each entry in the channels
  for (const [treeId, users] of channels.entries()) {
    // Map each user's m field to a string and store in the result
    result.set(treeId, users.map(({m, ws,...rest}) => ({...rest, m: JSON.stringify(m)})));
  }

  return result;
}