var _gingko$client$Native_Sha1 = function()
{
  // My addition
  var shared = new Uint32Array(80);

  // A pure JS implementation of sha1 for non-node environments.
  // TODO: port to elm
  function create(sync) {
    var h0 = 0x67452301;
    var h1 = 0xEFCDAB89;
    var h2 = 0x98BADCFE;
    var h3 = 0x10325476;
    var h4 = 0xC3D2E1F0;
    // The first 64 bytes (16 words) is the data chunk
    var block, offset = 0, shift = 24;
    var totalLength = 0;
    if (sync) block = shared;
    else block = new Uint32Array(80);

    return { update: update, digest: digest };

    // The user gave us more data.  Store it!
    function update(chunk) {
      if (typeof chunk === "string") return updateString(chunk);
      var length = chunk.length;
      totalLength += length * 8;
      for (var i = 0; i < length; i++) {
        write(chunk[i]);
      }
    }

    function updateString(string) {
      var length = string.length;
      totalLength += length * 8;
      for (var i = 0; i < length; i++) {
        write(string.charCodeAt(i));
      }
    }


    function write(byte) {
      block[offset] |= (byte & 0xff) << shift;
      if (shift) {
        shift -= 8;
      }
      else {
        offset++;
        shift = 24;
      }
      if (offset === 16) processBlock();
    }

    // No more data will come, pad the block, process and return the result.
    function digest() {
      // Pad
      write(0x80);
      if (offset > 14 || (offset === 14 && shift < 24)) {
        processBlock();
      }
      offset = 14;
      shift = 24;

      // 64-bit length big-endian
      write(0x00); // numbers this big aren't accurate in javascript anyway
      write(0x00); // ..So just hard-code to zero.
      write(totalLength > 0xffffffffff ? totalLength / 0x10000000000 : 0x00);
      write(totalLength > 0xffffffff ? totalLength / 0x100000000 : 0x00);
      for (var s = 24; s >= 0; s -= 8) {
        write(totalLength >> s);
      }

      // At this point one last processBlock() should trigger and we can pull out the result.
      return toHex(h0) +
             toHex(h1) +
             toHex(h2) +
             toHex(h3) +
             toHex(h4);
    }

    // We have a full block to process.  Let's do it!
    function processBlock() {
      // Extend the sixteen 32-bit words into eighty 32-bit words:
      for (var i = 16; i < 80; i++) {
        var w = block[i - 3] ^ block[i - 8] ^ block[i - 14] ^ block[i - 16];
        block[i] = (w << 1) | (w >>> 31);
      }

      // log(block);

      // Initialize hash value for this chunk:
      var a = h0;
      var b = h1;
      var c = h2;
      var d = h3;
      var e = h4;
      var f, k;

      // Main loop:
      for (i = 0; i < 80; i++) {
        if (i < 20) {
          f = d ^ (b & (c ^ d));
          k = 0x5A827999;
        }
        else if (i < 40) {
          f = b ^ c ^ d;
          k = 0x6ED9EBA1;
        }
        else if (i < 60) {
          f = (b & c) | (d & (b | c));
          k = 0x8F1BBCDC;
        }
        else {
          f = b ^ c ^ d;
          k = 0xCA62C1D6;
        }
        var temp = (a << 5 | a >>> 27) + f + e + k + (block[i]|0);
        e = d;
        d = c;
        c = (b << 30 | b >>> 2);
        b = a;
        a = temp;
      }

      // Add this chunk's hash to result so far:
      h0 = (h0 + a) | 0;
      h1 = (h1 + b) | 0;
      h2 = (h2 + c) | 0;
      h3 = (h3 + d) | 0;
      h4 = (h4 + e) | 0;

      // The block is now reusable.
      offset = 0;
      for (i = 0; i < 16; i++) {
        block[i] = 0;
      }
    }

    function toHex(word) {
      var hex = "";
      for (var i = 28; i >= 0; i -= 4) {
        hex += ((word >> i) & 0xf).toString(16);
      }
      return hex;
    }
  }

  /*================ axosoft/diff3 =============*/
/*
 * URL: https://github.com/cubicdaiya/onp
 *
 * Copyright (c) 2013 Tatsuhiko Kubo <cubicdaiya@gmail.com>
 * Copyright (c) 2016 Axosoft, LLC (www.axosoft.com)
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

/**
 * The algorithm implemented here is based on "An O(NP) Sequence Comparison Algorithm"
 * by described by Sun Wu, Udi Manber and Gene Myers
*/
var onp = function (a_, b_) {
    var a          = a_,
        b          = b_,
        m          = a.length,
        n          = b.length,
        reverse    = false,
        ed         = null,
        offset     = m + 1,
        path       = [],
        pathposi   = [],
        ses        = [],
        lcs        = "",
        SES_DELETE = -1,
        SES_COMMON = 0,
        SES_ADD    = 1;

    var tmp1,
        tmp2;

    var init = function () {
        if (m >= n) {
            tmp1    = a;
            tmp2    = m;
            a       = b;
            b       = tmp1;
            m       = n;
            n       = tmp2;
            reverse = true;
            offset = m + 1;
        }
    };

    var P = function (x, y, k) {
        return {
            'x' : x,
            'y' : y,
            'k' : k,
        };
    };

    var seselem = function (elem, t) {
        return {
            'elem' : elem,
            't'    : t,
        };
    };

    var snake = function (k, p, pp) {
        var r, x, y;
        if (p > pp) {
            r = path[k-1+offset];
        } else {
            r = path[k+1+offset];
        }

        y = Math.max(p, pp);
        x = y - k;
        while (x < m && y < n && a[x] === b[y]) {
            ++x;
            ++y;
        }

        path[k+offset] = pathposi.length;
        pathposi[pathposi.length] = new P(x, y, r);
        return y;
    };

    var recordseq = function (epc) {
        var x_idx, y_idx, px_idx, py_idx, i;
        x_idx  = y_idx  = 1;
        px_idx = py_idx = 0;
        for (i=epc.length-1;i>=0;--i) {
            while(px_idx < epc[i].x || py_idx < epc[i].y) {
                if (epc[i].y - epc[i].x > py_idx - px_idx) {
                    if (reverse) {
                        ses[ses.length] = new seselem(b[py_idx], SES_DELETE);
                    } else {
                        ses[ses.length] = new seselem(b[py_idx], SES_ADD);
                    }
                    ++y_idx;
                    ++py_idx;
                } else if (epc[i].y - epc[i].x < py_idx - px_idx) {
                    if (reverse) {
                        ses[ses.length] = new seselem(a[px_idx], SES_ADD);
                    } else {
                        ses[ses.length] = new seselem(a[px_idx], SES_DELETE);
                    }
                    ++x_idx;
                    ++px_idx;
                } else {
                    ses[ses.length] = new seselem(a[px_idx], SES_COMMON);
                    lcs += a[px_idx];
                    ++x_idx;
                    ++y_idx;
                    ++px_idx;
                    ++py_idx;
                }
            }
        }
    };

    init();

    return {
        SES_DELETE : -1,
        SES_COMMON :  0,
        SES_ADD    :  1,
        editdistance : function () {
            return ed;
        },
        getlcs : function () {
            return lcs;
        },
        getses : function () {
            return ses;
        },
        compose : function () {
            var delta, size, fp, p, r, epc, i, k;
            delta  = n - m;
            size   = m + n + 3;
            fp     = {};
            for (i=0;i<size;++i) {
                fp[i] = -1;
                path[i] = -1;
            }
            p = -1;
            do {
                ++p;
                for (k=-p;k<=delta-1;++k) {
                    fp[k+offset] = snake(k, fp[k-1+offset]+1, fp[k+1+offset]);
                }
                for (k=delta+p;k>=delta+1;--k) {
                    fp[k+offset] = snake(k, fp[k-1+offset]+1, fp[k+1+offset]);
                }
                fp[delta+offset] = snake(delta, fp[delta-1+offset]+1, fp[delta+1+offset]);
            } while (fp[delta+offset] !== n);

            ed = delta + 2 * p;

            r = path[delta+offset];

            epc  = [];
            while (r !== -1) {
                epc[epc.length] = new P(pathposi[r].x, pathposi[r].y, null);
                r = pathposi[r].k;
            }
            recordseq(epc);
        }
    };
};

// Copyright (c) 2006, 2008 Tony Garnock-Jones <tonyg@lshift.net>
// Copyright (c) 2006, 2008 LShift Ltd. <query@lshift.net>
// Copyright (c) 2016 Axosoft, LLC (www.axosoft.com)
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation files
// (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

function longestCommonSubsequence(file1, file2) {
  var diff = new onp(file1, file2);
  diff.compose();
  var ses = diff.getses();

  var root;
  var prev;
  var file1RevIdx = file1.length - 1,
      file2RevIdx = file2.length - 1;
  for (var i = ses.length - 1; i >= 0; --i) {
      if (ses[i].t === diff.SES_COMMON) {
        if (prev) {
          prev.chain = {
            file1index: file1RevIdx,
            file2index: file2RevIdx,
            chain: null
          };
          prev = prev.chain;
        } else {
          root = {
            file1index: file1RevIdx,
            file2index: file2RevIdx,
            chain: null
          };
          prev = root;
        }
        file1RevIdx--;
        file2RevIdx--;
      } else if (ses[i].t === diff.SES_DELETE) {
        file1RevIdx--;
      } else if (ses[i].t === diff.SES_ADD) {
        file2RevIdx--;
      }
  }

  var tail = {
    file1index: -1,
    file2index: -1,
    chain: null
  };

  if (!prev) {
    return tail;
  }

  prev.chain = tail;

  return root;
}

function diffIndices(file1, file2) {
  // We apply the LCS to give a simple representation of the
  // offsets and lengths of mismatched chunks in the input
  // files. This is used by diff3_merge_indices below.

  var result = [];
  var tail1 = file1.length;
  var tail2 = file2.length;

  for (var candidate = longestCommonSubsequence(file1, file2); candidate !== null; candidate = candidate.chain) {
    var mismatchLength1 = tail1 - candidate.file1index - 1;
    var mismatchLength2 = tail2 - candidate.file2index - 1;
    tail1 = candidate.file1index;
    tail2 = candidate.file2index;

    if (mismatchLength1 || mismatchLength2) {
      result.push({
        file1: [tail1 + 1, mismatchLength1],
        file2: [tail2 + 1, mismatchLength2]
      });
    }
  }

  result.reverse();
  return result;
}

function diff3MergeIndices(a, o, b) {
  // Given three files, A, O, and B, where both A and B are
  // independently derived from O, returns a fairly complicated
  // internal representation of merge decisions it's taken. The
  // interested reader may wish to consult
  //
  // Sanjeev Khanna, Keshav Kunal, and Benjamin C. Pierce. "A
  // Formal Investigation of Diff3." In Arvind and Prasad,
  // editors, Foundations of Software Technology and Theoretical
  // Computer Science (FSTTCS), December 2007.
  //
  // (http://www.cis.upenn.edu/~bcpierce/papers/diff3-short.pdf)
  var i;

  var m1 = diffIndices(o, a);
  var m2 = diffIndices(o, b);

  var hunks = [];

  function addHunk(h, side) {
    hunks.push([h.file1[0], side, h.file1[1], h.file2[0], h.file2[1]]);
  }
  for (i = 0; i < m1.length; i++) {
    addHunk(m1[i], 0);
  }
  for (i = 0; i < m2.length; i++) {
    addHunk(m2[i], 2);
  }
  hunks.sort(function(x, y) {
    return x[0] - y[0]
  });

  var result = [];
  var commonOffset = 0;

  function copyCommon(targetOffset) {
    if (targetOffset > commonOffset) {
      result.push([1, commonOffset, targetOffset - commonOffset]);
      commonOffset = targetOffset;
    }
  }

  for (var hunkIndex = 0; hunkIndex < hunks.length; hunkIndex++) {
    var firstHunkIndex = hunkIndex;
    var hunk = hunks[hunkIndex];
    var regionLhs = hunk[0];
    var regionRhs = regionLhs + hunk[2];
    while (hunkIndex < hunks.length - 1) {
      var maybeOverlapping = hunks[hunkIndex + 1];
      var maybeLhs = maybeOverlapping[0];
      if (maybeLhs > regionRhs) break;
      regionRhs = Math.max(regionRhs, maybeLhs + maybeOverlapping[2]);
      hunkIndex++;
    }

    copyCommon(regionLhs);
    if (firstHunkIndex == hunkIndex) {
      // The "overlap" was only one hunk long, meaning that
      // there's no conflict here. Either a and o were the
      // same, or b and o were the same.
      if (hunk[4] > 0) {
        result.push([hunk[1], hunk[3], hunk[4]]);
      }
    } else {
      // A proper conflict. Determine the extents of the
      // regions involved from a, o and b. Effectively merge
      // all the hunks on the left into one giant hunk, and
      // do the same for the right; then, correct for skew
      // in the regions of o that each side changed, and
      // report appropriate spans for the three sides.
      var regions = {
        0: [a.length, -1, o.length, -1],
        2: [b.length, -1, o.length, -1]
      };
      for (i = firstHunkIndex; i <= hunkIndex; i++) {
        hunk = hunks[i];
        var side = hunk[1];
        var r = regions[side];
        var oLhs = hunk[0];
        var oRhs = oLhs + hunk[2];
        var abLhs = hunk[3];
        var abRhs = abLhs + hunk[4];
        r[0] = Math.min(abLhs, r[0]);
        r[1] = Math.max(abRhs, r[1]);
        r[2] = Math.min(oLhs, r[2]);
        r[3] = Math.max(oRhs, r[3]);
      }
      var aLhs = regions[0][0] + (regionLhs - regions[0][2]);
      var aRhs = regions[0][1] + (regionRhs - regions[0][3]);
      var bLhs = regions[2][0] + (regionLhs - regions[2][2]);
      var bRhs = regions[2][1] + (regionRhs - regions[2][3]);
      result.push([-1,
        aLhs, aRhs - aLhs,
        regionLhs, regionRhs - regionLhs,
        bLhs, bRhs - bLhs
      ]);
    }
    commonOffset = regionRhs;
  }

  copyCommon(o.length);
  return result;
}

function diff3Merge(a, o, b) {
  // Applies the output of Diff.diff3_merge_indices to actually
  // construct the merged file; the returned result alternates
  // between "ok" and "conflict" blocks.

  var elmListToJs = function(l) {
    var ll = []

    var listToJsRecurse = function(j){
      if(j["ctor"] == "::") {
        ll.push(j["_0"])
        listToJsRecurse(j["_1"])
      }
    }

    listToJsRecurse(l)
    return ll
  }

  a = elmListToJs(a)
  o = elmListToJs(o)
  b = elmListToJs(b)


  var result = [];
  var files = [a, o, b];
  var indices = diff3MergeIndices(a, o, b);

  var okLines = [];

  function flushOk() {
    if (okLines.length) {
      result.push({
        ok: okLines
      });
    }
    okLines = [];
  }

  function pushOk(xs) {
    for (var j = 0; j < xs.length; j++) {
      okLines.push(xs[j]);
    }
  }

  function isTrueConflict(rec) {
    if (rec[2] != rec[6]) return true;
    var aoff = rec[1];
    var boff = rec[5];
    for (var j = 0; j < rec[2]; j++) {
      if (a[j + aoff] != b[j + boff]) return true;
    }
    return false;
  }

  for (var i = 0; i < indices.length; i++) {
    var x = indices[i];
    var side = x[0];
    if (side == -1) {
      if (!isTrueConflict(x)) {
        pushOk(files[0].slice(x[1], x[1] + x[2]));
      } else {
        flushOk();
        result.push({
          conflict: {
            a: a.slice(x[1], x[1] + x[2]),
            aIndex: x[1],
            o: o.slice(x[3], x[3] + x[4]),
            oIndex: x[3],
            b: b.slice(x[5], x[5] + x[6]),
            bIndex: x[5]
          }
        });
      }
    } else {
      pushOk(files[side].slice(x[1], x[1] + x[2]));
    }
  }

  flushOk();

  var arrayToElmList = function(arr) {
    var list = _elm_lang$core$Native_List.Nil

    for (var i = arr.length; i--;) {
      list = _elm_lang$core$Native_List.Cons(arr[i], list)
    }
    return list
  }

  var elmResult = result.map(r => {
    if(r.ok) {
      var okList = _elm_lang$core$Native_List.Nil
      for (var i = r.ok.length; i--;)
      {
        okList = _elm_lang$core$Native_List.Cons(r.ok[i], okList)
      }
      return {"ctor": "DiffOk", "_0": okList}
    } else {
      var confListA = arrayToElmList(r.conflict.a)
      var confListO = arrayToElmList(r.conflict.o)
      var confListB = arrayToElmList(r.conflict.b)

      return {"ctor": "DiffConflict", "_0": {"ctor": "_Tuple3", "_0":confListA, "_1":confListO, "_2":confListB}}
    }
  })
  console.log(result,elmResult)
  return arrayToElmList(elmResult);
}


  /*============================================*/
  return {
    sha1: function (buffer) {
      var shasum = create(true);
      shasum.update(buffer);
      return shasum.digest();
    }
  , timestamp: function(a) {return Date.now()}
  , timeJSON: function(a) {return new Date().toJSON()}
  , diff3Merge : F3(diff3Merge)
  };

}();
