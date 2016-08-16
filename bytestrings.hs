import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S

-- Word8 type represents a number from 0 to 255
-- A ByteString will return Chunks of 64k
a1 = B.pack [99,97,110]  
a2 = B.pack [98..120]
a3 = B.unpack a1

-- takes a list of strict bytestrings
b1 = B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]

-- cons takes a byte and a bytestring and puts the byte at the beginning
c1 = B.cons 85 $ B.pack [80,81,82,84]  
c2 = B.cons' 85 $ B.pack [80,81,82,84]  
c3 = foldr B.cons B.empty [50..60]  
c4 = foldr B.cons' B.empty [50..60]
