#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <math.h>
#include "rrScanner.h"
#include "rrException.h"
#include "rrStringUtils.h"
//---------------------------------------------------------------------------

namespace rr
{
Scanner::Scanner()
:
EOFCHAR('\x7F'),
CR((char) 13),
LF((char) 10),
bufferLength(),
bufferPtr(),
ftoken(tEmptyToken),
yylineno(),
timeWord1("time"),
timeWord2("Time"),
timeWord3("TIME"),
FromQueue(true),
IgnoreNewLines(true),
fch('\0'),
tokenDouble(),
tokenInteger(),
tokenScalar(), // Used to retrieve int or double
tokenString(),
pStream(NULL),
previousToken(CodeTypes::tEmptyToken),
currentToken(CodeTypes::tEmptyToken)
{
    wordTable;
    tokenQueue;
    FCharTable.resize(255);
    buffer.resize(255);
    initScanner();
}

void Scanner::AssignStream(stringstream& str)
{
    pStream = &str;
}

void Scanner::initScanner()
{
    char ch;
//    for (ch = '\x00'; ch < '\xFF'; ch++)
//    {
//        FCharTable[ch] = TCharCode::cSPECIAL;
//    }

    for (int i = 0; i < 48; i++)
    {
        FCharTable[i] = TCharCode::cSPECIAL;
    }

    for (int i = 57; i < 63; i++)
    {
        char ch = i;

        FCharTable[i] = TCharCode::cSPECIAL;
    }
    for (ch = '0'; ch <= '9'; ch++)
    {
        FCharTable[ch] = TCharCode::cDIGIT;
    }

    for (ch = 'A'; ch <= 'Z'; ch++)
    {
        FCharTable[ch] = TCharCode::cLETTER;
    }

    for (ch = 'a'; ch <= 'z'; ch++)
    {
        FCharTable[ch] = TCharCode::cLETTER;
    }

    FCharTable['='] = TCharCode::cSPECIAL;

    FCharTable['.'] = TCharCode::cPOINT;
    FCharTable['"'] = TCharCode::cDOUBLEQUOTE;
    FCharTable['_'] = TCharCode::cUNDERSCORE;
    FCharTable['\t'] = TCharCode::cWHITESPACE;
    FCharTable[' '] = TCharCode::cWHITESPACE;
    FCharTable[EOFCHAR] = TCharCode::cETX;


    wordTable["and"]  = CodeTypes::tAndToken;
    wordTable["or"]  = CodeTypes::tOrToken;
    wordTable["not"] = CodeTypes::tNotToken;
    wordTable["xor"] = CodeTypes::tXorToken;

    wordTable[timeWord1] = CodeTypes::tTimeWord1;
    wordTable[timeWord2] = CodeTypes::tTimeWord2;
    wordTable[timeWord3] = CodeTypes::tTimeWord3;
}


// Create a readonly property for the current line number
int Scanner::lineNumber()
{
    return yylineno;
}

// readonly current token property
CodeTypes Scanner::token()
{
    return ftoken;
}


// Must be called before using nextToken()
void Scanner::startScanner()
{
    yylineno = 1;
    bufferPtr = 0;
    nextChar();
}

char Scanner::getCharFromBuffer()
{
    if(!pStream)
    {
        throw Exception("There is no stream assigned to the scanner.. exiting");
    }
    // If the buffer is empty, read a new chuck of text from the
    // input stream, this might be a stream or console
    if (bufferPtr == 0)
    {
        // Read a chunck of data from the input stream
        //        bufferLength = (char) FStream.Read(buffer, 0, 255);
        char chars[255];

        pStream ->read(chars, 255);

        bufferLength = pStream->gcount();//(char) pStream->read(buffer, 0, 255);
        if (bufferLength == 0)
        {
            return EOFCHAR;
        }

        copy(chars, chars + bufferLength, buffer.begin());
    }

    char ch = (char) buffer[bufferPtr];
    bufferPtr++;
    if (bufferPtr >= bufferLength)
    {
        bufferPtr = 0; // Indicates the buffer is empty
    }
    return ch;
}

// -------------------------------------------------------------------
// Fetches next character from input stream and filters NL if required
// -------------------------------------------------------------------
char Scanner::nextChar()
{
    fch = getCharFromBuffer();
    if (IgnoreNewLines)
    {
        // Turn any CFs or LFs into space characters
        if (fch == CR)
        {
            yylineno++;
            fch = ' ';
            return fch;
        }

        if (fch == LF)
            fch = ' ';
        return fch;
    }
    else
    {
        if (fch == CR)
            yylineno++;
    }
    return fch;
}

// -------------------------------------------------------------------
// Skips any blanks, ie TAB, ' '
// -------------------------------------------------------------------

void Scanner::skipBlanks()
{
    while (FCharTable[fch] == TCharCode::cWHITESPACE)
    {
        if ((fch == LF) || (fch == CR))
            return;
        nextChar();
    }
}

// -------------------------------------------------------------------
// Scan for a word, words start with letter or underscore then continue
// with letters, digits or underscore
// -------------------------------------------------------------------
void Scanner::getWord()
{
    while ((FCharTable[fch] == TCharCode::cLETTER)
           || (FCharTable[fch] == TCharCode::cDIGIT)
           || (FCharTable[fch] == TCharCode::cUNDERSCORE))
    {
        tokenString = tokenString + fch; // Inefficient but convenient
        nextChar();
    }

    if (wordTable.find(tokenString) != wordTable.end())
    {
        try
        {
            ftoken = (CodeTypes) wordTable[tokenString];
        }
        catch (Exception)
        {
            ftoken = CodeTypes::tWordToken;
        }
    }
    else
    {
      ftoken = CodeTypes::tWordToken;
    }
}

// -------------------------------------------------------------------
// Scan for a number: an integer, double or complex (eg 3i)
// -------------------------------------------------------------------

void Scanner::getNumber()
{
    const int MAX_DIGIT_COUNT = 3; // Max number of digits in exponent

    int single_digit;
    double scale;
    double evalue;
    int exponent_sign;
    int digit_count;

    tokenInteger = 0;
    tokenDouble = 0.0;
    tokenScalar = 0.0;
    evalue = 0.0;
    exponent_sign = 1;

    // Assume first it's an integer
    ftoken = CodeTypes::tIntToken;

    // Pick up number before any decimal place
    if (fch != '.')
        try
        {
            do
            {
                single_digit = fch - '0';
                tokenInteger = 10*tokenInteger + single_digit;
                tokenScalar = tokenInteger;
                nextChar();
            } while (FCharTable[fch] == TCharCode::cDIGIT);
        }
        catch(const RRException())
        {
            throw RRException("Integer Overflow - constant value too large to read");
        }

    scale = 1;
    if (fch == '.')
    {
        // Then it's a float. Start collecting fractional part
        ftoken = CodeTypes::tDoubleToken;
        tokenDouble = tokenInteger;
        nextChar();
        if (FCharTable[fch] != TCharCode::cDIGIT)
        {
            throw ScannerException("Syntax error: expecting number after decimal point");
        }

        try
        {
            while (FCharTable[fch] == TCharCode::cDIGIT)
            {
                scale = scale*0.1;
                single_digit = fch - '0';
                tokenDouble = tokenDouble + (single_digit*scale);
                tokenScalar = tokenDouble;
                nextChar();
            }
        }
        catch(const Exception())
        {
            throw new ScannerException("Floating point overflow - constant value too large to read in");
        }
    }

    // Next check for scientific notation
    if ((fch == 'e') || (fch == 'E'))
    {
        // Then it's a float. Start collecting exponent part
        if (ftoken == CodeTypes::tIntToken)
        {
            ftoken = CodeTypes::tDoubleToken;
            tokenDouble = tokenInteger;
            tokenScalar = tokenInteger;
        }
        nextChar();
        if ((fch == '-') || (fch == '+'))
        {
            if (fch == '-') exponent_sign = -1;
            nextChar();
        }
        // accumulate exponent, check that first ch is a digit
        if (FCharTable[fch] != TCharCode::cDIGIT)
            throw new ScannerException("Syntax error: number expected in exponent");

        digit_count = 0;
        try
        {
            do
            {
                digit_count++;
                single_digit = fch - '0';
                evalue = 10*evalue + single_digit;
                nextChar();
            } while ((FCharTable[fch] == TCharCode::cDIGIT) && (digit_count <= MAX_DIGIT_COUNT));
        }
        catch(const Exception())
        {
            throw new ScannerException("Floating point overflow - Constant value too large to read");
        }

        if (digit_count > MAX_DIGIT_COUNT)
            throw new ScannerException("Syntax error: too many digits in exponent");

        evalue = evalue*exponent_sign;
        if (evalue > 300)
        {
            throw new ScannerException("Exponent overflow while parsing floating point number");
        }
        evalue = pow(10.0, evalue);
        tokenDouble = tokenDouble*evalue;
        tokenScalar = tokenDouble;
    }

    // Check for complex number
    if ((fch == 'i') || (fch == 'j'))
    {
        if (ftoken == CodeTypes::tIntToken)
            tokenDouble = tokenInteger;
        ftoken = CodeTypes::tComplexToken;
        nextChar();
    }
}


// Returns true if the character ch is a double quote
bool Scanner::IsDoubleQuote(char ch)
{
    if (FCharTable[ch] == TCharCode::cDOUBLEQUOTE)
        return true;
    else return false;
}

// -------------------------------------------------------------------
// Scan for a string, eg "abc"
// -------------------------------------------------------------------

void Scanner::getString()
{
    bool OldIgnoreNewLines;
    tokenString = "";
    nextChar();

    ftoken = CodeTypes::tStringToken;
    while (fch != EOFCHAR)
    {
        // Check for escape characters
        if (fch == '\\')
        {
            nextChar();
            switch (fch)
            {
                case '\\':
                    tokenString = tokenString + '\\';
                    break;
                case 'n':
                    tokenString = tokenString + CR + LF;
                    break;
                case 'r':
                    tokenString = tokenString + CR;
                    break;
                case 'f':
                    tokenString = tokenString + LF;
                    break;
                case 't':
                    tokenString = tokenString + string(' ', 6);
                    break;
                default:
                    throw new ScannerException("Syntax error: Unrecognised control code in string");
            }
            nextChar();
        }
        else
        {
            OldIgnoreNewLines = IgnoreNewLines;
            if (IsDoubleQuote(fch))
            {
                // Just in case the double quote is at the end of a line and another string
                // start immediately in the next line, if we ignore newlines we'll
                // pick up a double quote rather than the end of a string
                IgnoreNewLines = false;
                nextChar();
                if (IsDoubleQuote(fch))
                {
                    tokenString = tokenString + fch;
                    nextChar();
                }
                else
                {
                    if (OldIgnoreNewLines)
                    {
                        while (fch == CR)
                        {
                            nextChar();
                            while (fch == LF)
                                nextChar();
                        }
                    }
                    IgnoreNewLines = OldIgnoreNewLines;
                    return;
                }
            }
            else
            {
                tokenString = tokenString + fch;
                nextChar();
            }
            IgnoreNewLines = OldIgnoreNewLines;
        }
    }
    if (fch == EOFCHAR)
        throw new ScannerException("Syntax error: String without terminating quotation mark");
}

// -------------------------------------------------------------------
// Scan for special characters
// -------------------------------------------------------------------
void Scanner::getSpecial()
{
    char tch;
    switch (fch)
    {
        case '\13':
            ftoken = CodeTypes::tEolToken;
            nextChar();
            break;

        case ';':
            ftoken = CodeTypes::tSemiColonToken;
            nextChar();
            break;

        case ',':
            ftoken = CodeTypes::tCommaToken;
            nextChar();
            break;

        case ':':
            ftoken = CodeTypes::tColonToken;
            nextChar();
            break;

        case '=':
            nextChar();
            if (fch == '>')
            {
                ftoken = CodeTypes::tReversibleArrow;
                nextChar();
            }
            else
                ftoken = CodeTypes::tEqualsToken;
            break;

        case '+':
            ftoken = CodeTypes::tPlusToken;
            nextChar();
            break;

        case '-':
            nextChar();
            if (fch == '>')
            {
                ftoken = CodeTypes::tIrreversibleArrow;
                nextChar();
            }
            else
                ftoken = CodeTypes::tMinusToken;
            break;

        case '*':
            nextChar();
            ftoken = CodeTypes::tMultToken;
            break;

        case '/': // look ahead at next ch
            tch = nextChar();
            if (tch == '/')
            {
                ftoken = CodeTypes::tStartComment;
                nextChar();
            }
            else
                ftoken = CodeTypes::tDivToken;
            break;

        case '(':
            nextChar();
            ftoken = CodeTypes::tLParenToken;
            break;

        case ')':
            nextChar();
            ftoken = CodeTypes::tRParenToken;
            break;

        case '[':
            nextChar();
            ftoken = CodeTypes::tLBracToken;
            break;

        case ']':
            nextChar();
            ftoken = CodeTypes::tRBracToken;
            break;

        case '{':
            nextChar();
            ftoken = CodeTypes::tLCBracToken;
            break;

        case '}':
            nextChar();
            ftoken = CodeTypes::tRCBracToken;
            break;

        case '^':
            nextChar();
            ftoken = CodeTypes::tPowerToken;
            break;

        case '<':
            nextChar();
            if (fch == '=')
            {
                ftoken = CodeTypes::tLessThanOrEqualToken;
                nextChar();
            }
            else
                ftoken = CodeTypes::tLessThanToken;
            break;

        case '>':
            nextChar();
            if (fch == '=')
            {
                ftoken = CodeTypes::tMoreThanOrEqualToken;
                nextChar();
            }
            else
                ftoken = CodeTypes::tMoreThanToken;
            break;

        case '!':
            nextChar();
            if (fch == '=')
            {
                ftoken = CodeTypes::tNotEqualToken;
                nextChar();
            }
            break;

        case '.':
            nextChar();
            ftoken = CodeTypes::tPointToken;
            break;

        case '$':
            nextChar();
            ftoken = CodeTypes::tDollarToken;
            break;

        default:
        {
            string aToken;
            aToken.push_back(fch);
            throw ScannerException("Syntax error: Unknown special token [" + aToken  + "]");
        }
    }
}

// -------------------------------------------------------------------
// This scanner has a simple queue mechanism that allows one to put
// tokens back to the scanner via a quaue
// -------------------------------------------------------------------
bool Scanner::IsQueueEmpty()
{
    return (tokenQueue.size() == 0);
}

// -------------------------------------------------------------------
// Add the current token to the queue
// -------------------------------------------------------------------
void Scanner::AddTokenToQueue()
{
    Token t;// = new Token();
    t.tokenCode     = ftoken;
    t.tokenDouble     = tokenDouble;
    t.tokenInteger     = tokenInteger;
    t.tokenString     = tokenString;
    t.tokenValue     = tokenScalar;
    tokenQueue.push(t);// push or ??    tokenQueue.Enqueue(t);
}

// -------------------------------------------------------------------
// Get a token from the queue
// Check that a token is in the queue first by calling IsQueueEmpty()
// Used internally
// -------------------------------------------------------------------
void Scanner::getTokenFromQueue()
{
//    Token t = (Token) tokenQueue.Dequeue();
    Token t = (Token)     tokenQueue.front();
    tokenQueue.pop();
    ftoken         = t.tokenCode;
    tokenString = t.tokenString;
    tokenScalar = t.tokenValue;
    tokenInteger = t.tokenInteger;
    tokenDouble = t.tokenDouble;
}

// End of Queue routines
// -------------------------------------------------------------------
void Scanner::nextTokenInternal()
{
    // check if a token has been pushed back into the token stream, if so use it first
    // I think I should get rid of this code, use queue methods instead
    if (previousToken.tokenCode != CodeTypes::tEmptyToken)
    {
        ftoken = previousToken.tokenCode;
        tokenString = previousToken.tokenString;
        tokenDouble = previousToken.tokenDouble;
        tokenInteger = previousToken.tokenInteger;
        previousToken.tokenCode = CodeTypes::tEmptyToken;
        return;
    }

    // Check if there is anything in the token queue, if so get the item
    // from the queue and exit. If not, read as normal from the stream.
    // Checking the queue before reading from the stream can be turned off and on
    // by setting the FromQueue Flag.
    if(FromQueue)
    {
        if(!IsQueueEmpty())
        {
            getTokenFromQueue();
            return;
        }
    }

    skipBlanks();
    tokenString = "";

    TCharCode code = FCharTable[fch];
    switch(code)
    {
        case TCharCode::cLETTER:
        case TCharCode::cUNDERSCORE:
            getWord();
            break;
        case TCharCode::cDIGIT:
            getNumber();
            break;
        case TCharCode::cDOUBLEQUOTE:
            getString();
            break;
        case TCharCode::cETX:
            ftoken = CodeTypes::tEndOfStreamToken;
            break;
        default:
            getSpecial();
            break;
    }
}

// -------------------------------------------------------------------
// Retrieve the next token in the stream, return tEndOfStreamToken
// if it reaches the end of the stream
// -------------------------------------------------------------------

void Scanner::nextToken()
{
    nextTokenInternal();
    while (ftoken == CodeTypes::tStartComment)
    {
        // Comment ends with an end of line char
        while ((fch != LF) && (fch != EOFCHAR))
            fch = getCharFromBuffer();
        //while ((fch != CR) && (fch != EOFCHAR))
        //      fch = getCharFromBuffer();
        //while ((fch == LF) && (fch !=  EOFCHAR))
        //      fch = getCharFromBuffer();
        while (fch == LF)
        {
            yylineno++;
            //while (fch == LF)
            //      nextChar();  // Dump the linefeed
            fch = nextChar();
        }
        nextTokenInternal(); // get the real next token
    }
}

// -------------------------------------------------------------------
// Allows one token look ahead
// Push token back into token stream
// -------------------------------------------------------------------
void Scanner::UnGetToken()
{
    previousToken.tokenCode = ftoken;
    previousToken.tokenString = tokenString;
    previousToken.tokenInteger = tokenInteger;
    previousToken.tokenDouble = tokenDouble;
}

// -------------------------------------------------------------------
// Given a token, this function returns the string eqauivalent
// -------------------------------------------------------------------
string Scanner::tokenToString(const CodeTypes& code)
{
    switch (code)
    {
        case CodeTypes::tIntToken:
            return "<Integer: " + ToString(tokenInteger) + ">";
        case CodeTypes::tDoubleToken:
            return "<Double: " + ToString(tokenDouble) + ">";
        case CodeTypes::tComplexToken:
            return "<Complex: " + ToString(tokenDouble) + "i>";
        case CodeTypes::tStringToken:
            return "<String: " + tokenString + ">";
        case CodeTypes::tWordToken:
            return "<Identifier: " + tokenString + ">";
        case CodeTypes::tEndOfStreamToken:
            return "<end of stream>";
        case CodeTypes::tEolToken:
            return "<EOLN>";
        case CodeTypes::tSemiColonToken:
            return ";";
        case CodeTypes::tCommaToken:
            return ",";
        case CodeTypes::tEqualsToken:
            return "=";
        case CodeTypes::tPlusToken:
            return "+";
        case CodeTypes::tMinusToken:
            return "-";
        case CodeTypes::tMultToken:
            return "*";
        case CodeTypes::tDivToken:
            return "/";
        case CodeTypes::tPowerToken:
            return "^";
        case CodeTypes::tLParenToken:
            return "(";
        case CodeTypes::tRParenToken:
            return ")";
        case CodeTypes::tLBracToken:
            return "[";
        case CodeTypes::tRBracToken:
            return "]";
        case CodeTypes::tLCBracToken:
            return "{";
        case CodeTypes::tRCBracToken:
            return "}";
        case CodeTypes::tDollarToken:
            return "$";
        case CodeTypes::tOrToken:
            return "or";
        case CodeTypes::tAndToken:
            return "and";
        case CodeTypes::tNotToken:
            return "not";
        case CodeTypes::tXorToken:
            return "xor";
        case CodeTypes::tLessThanToken:
            return "<";
        case CodeTypes::tLessThanOrEqualToken:
            return "<=";
        case CodeTypes::tMoreThanToken:
            return ">";
        case CodeTypes::tMoreThanOrEqualToken:
            return ">=";
        case CodeTypes::tNotEqualToken:
            return "!=";
        case CodeTypes::tReversibleArrow:
            return "=>";
        case CodeTypes::tIrreversibleArrow:
            return "->";
        case CodeTypes::tIfToken:
            return "if";
        case CodeTypes::tWhileToken:
            return "while";
        case CodeTypes::tdefnToken:
            return "defn";
        case CodeTypes::tEndToken:
            return "end";
        case CodeTypes::tInternalToken:
            return "Internal";
        case CodeTypes::tExternalToken:
            return "External";
        case CodeTypes::tParameterToken:
            return "Parameter";
        case CodeTypes::tTimeStartToken:
            return "TimeStart";
        case CodeTypes::tTimeEndToken:
            return "TimeEnd";
        case CodeTypes::tNumPointsToken:
            return "NumPoints";
        case CodeTypes::tSimulateToken:
            return "Simulate";

        default:
            return "<unknown>";
    }
}
}




