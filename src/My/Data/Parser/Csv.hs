{-# LANGUAGE OverloadedStrings #-}
module My.Data.Parser.Csv
  ( parseCsv
  ) where

import Data.Attoparsec.Text
import qualified Data.Text as T

-- ;; CSV definision by ABNF
-- ;; http://www.kasai.fm/wiki/rfc4180jp
-- ;; https://www.ietf.org/rfc/rfc4180.txt

-- file = [header CRLF] record *(CRLF record) [CRLF]
-- header = name *(COMMA name)
-- record = field *(COMMA field)
-- name = field
-- field = (escaped / non-escaped)
-- escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
-- non-escaped = *TEXTDATA
-- COMMA = %x2C
-- CR = %x0D ;as per section 6.1 of RFC 2234 [2]
-- DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
-- LF = %x0A ;as per section 6.1 of RFC 2234 [2]
-- CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
-- TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E

parseCsv = parseOnly (file <* endOfInput)

file = do
  h <- option [] (header <* eol)
  r <- (:) <$> record <*> many' (eol *> record)
  many' eol
  return (h:r)

header = (:) <$> name <*> many' (comma *> name)
record = (:) <$> field <*> many' (comma *> field)

name = field
field = choice [escaped, nonEscaped]

escaped = dquote *> many' (choice [textdata, comma, cr, lf, dquote *> dquote]) <* dquote
nonEscaped = many' textdata

comma    = char ','
cr       = char '\r'
dquote   = char '"'
lf       = char '\n'
crlf     = string "\r\n"
textdata = satisfy $ notInClass ",\r\"\n"

eol = choice [string "\n", crlf, string "\r"]
