/***********************************************************
  FileName:         SQLAPI.h
  Description:      本文件是SQLAPI提供的全部对外接口类信息
  Class List:
    1.  SAConnection        管理数据源会话的类
    2.  SACommand           执行数据源操作的类
    3. SAException         处理异常的类

***********************************************************/
#if !defined(_SQLAPI_H__INCLUDED_)
#define _SQLAPI_H__INCLUDED_

#define sa_min(x, y) ((x) < (y)? (x) : (y))
#define sa_max(x, y) ((x) > (y)? (x) : (y))

#ifdef SQLAPI_EXPORTS
    #define SQLAPI_API __declspec(dllexport)
#else
    #define SQLAPI_API
#endif

#include <time.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>


#ifdef WIN32
typedef __int64 sa_int64;
#else
typedef long long sa_int64;
#endif

typedef char SAChar;
    #define _TSA(x)      x
    #define sa_strlen(x) strlen(x)
    #define sa_strstr(x, y) strstr(x, y)
    #define sa_strchr(x, y) strchr(x, y)
    #define sa_isspace(x) isspace((unsigned char)x)
    #define sa_isdigit(x) isdigit((unsigned char)x)
    #define sa_isalpha(x) isalpha((unsigned char)x)
    #define sa_strcmp(x, y) strcmp(x, y)
    #define sa_strncmp(x, y, z) strncmp(x, y, z)
    #define sa_tolower(x) tolower((unsigned char)x)
    #define sa_strcoll(x, y) strcoll(x, y)
    #define sa_strpbrk(x, y) strpbrk(x, y)
    #define sa_strrchr(x, y) strrchr(x, y)
    #define sa_strtol(x, y, z) strtol(x, y, z)
    #define sa_strtoul(x, y, z) strtoul(x, y, z)
    #define sa_strtod(x, y) strtod(x, y)
    #define sa_toi(x) atoi(x)
    #define sa_tol(x) atol(x)
    #if defined(WIN32)    // windows
            #define sa_strtoi8(x) _atoi64(x)
    #elif defined(_HP_UNIX)
	        #define sa_strtoi8(x) strtol(x, (char**)0, 10)
	#elif defined(_HP_UNIX_ITANIUM) || defined(_IBM_UNIX)  // HP-UX AIX
	        #define sa_strtoi8(x) strtoll(x, (char**)0, 10)
	#else   // SunOS Linux
            #define sa_strtoi8(x) atoll(x)
    #endif
    #define sa_vsprintf(x, y, z) vsprintf(x, y, z)
    #define sa_sprintf sprintf
    #define sa_csinc(x) (++(x))
    #define sa_clen(x) (1)
///////////////////////////////

typedef unsigned char  BYTE;
typedef unsigned short WORD;
typedef unsigned long  DWORD_T;


#define SA_MAX_PARAM_VALUE_LENGTH         256
#define SA_MAX_PARAM_NAME_LENGTH          80
#define SA_MAX_PARAM_ROWS                 100000

#define SA_MAX_DBNAME_LEN                 50
#define SA_MAX_USERNAME_LEN               50
#define SA_MAX_PASSWORD_LEN               50
#define SA_MAX_COMMAND_TEXT_LEN           1024
class saAPI;
class ora8ConnectionHandles;
class Iora8Connection;
class ISAConnection;
class SAConnection;
///////////////////////////////

struct SAStringConvertedData
{
    int nDataLength;        // length of converted data (including terminator)
    // !SAChar ConvertedData[nDataLength+1]

#ifdef SA_UNICODE
    char *data()        // char * to converted data
        { return (char *)(this+1); }
#else   // !SA_UNICODE
    wchar_t *data() // wchar_t * to converted data
        { return (wchar_t *)(this+1); }
#endif  //!SA_UNICODE
};

struct SAStringData
{
    SAStringConvertedData   *pConvertedData;    // pointer to converted data, if any

#ifdef SA_UNICODE
    // nDataLength*sizeof(SAChar) - real length of binary data
    // also true: nLengthInBytes % sizeof(SAChar)
    // In bytes [0..sizeof(SAChar)-1]
    int nBinaryDataLengthDiff;
#endif  // SA_UNICODE

    long nRefs;             // reference count
    int nDataLength;        // length of data (including terminator)
    int nAllocLength;       // length of allocation
    // SAChar data[nAllocLength]

    SAChar *data()           // SAChar * to managed data
        { return (SAChar *)(this+1); }
};

class SQLAPI_API SAString
{
public:
// Constructors
    // constructs empty SAString
    SAString();
    // copy constructor
    SAString(const SAString &stringSrc);
    // from a single character
    SAString(SAChar ch, int nRepeat = 1);
    // from an ANSI (multibyte) string (converts to SAChar)
    SAString(const char *lpsz);
    // from a UNICODE string (converts to SAChar)
    SAString(const wchar_t *lpsz);
    // subset of characters from an ANSI (multibyte) string (converts to SAChar)
    SAString(const char *lpch, int nLength);
    // subset of characters from a UNICODE string (converts to SAChar)
    SAString(const wchar_t *lpch, int nLength);
    // from unsigned characters (converts to SAChar)
    SAString(const unsigned char *psz);
    // special constructor for binary data (no converion to SAChar)
    SAString(const void *pBuffer, int nLengthInBytes);

// Attributes & Operations

    // get data length (in characters)
    //int GetLength() const;
    inline int GetLength() const
    {
        return GetData()->nDataLength;
    }
    // true if zero length
    //bool IsEmpty() const;
    inline bool IsEmpty() const
    {
        return GetData()->nDataLength == 0;
    }
    // clear contents to empty
    void Empty();

    // return pointer to const string
    operator const SAChar *() const;

    // overloaded assignment

    // ref-counted copy from another SAString
    const SAString &operator =(const SAString &sSrc);
    // set string content to single character
    const SAString &operator=(SAChar ch);
#ifdef SA_UNICODE
    const SAString &operator=(char ch);
#endif
    // copy string content from ANSI (multibyte) string (converts to SAChar)
    const SAString &operator=(const char *lpsz);
    // copy string content from UNICODE string (converts to SAChar)
    const SAString &operator=(const wchar_t *lpsz);
    // copy string content from unsigned chars
    const SAString &operator=(const unsigned char *psz);

    // string concatenation

    // concatenate from another SAString
    const SAString &operator+=(const SAString &string);
    // concatenate a single character
    const SAString &operator+=(SAChar ch);
#ifdef SA_UNICODE
    // concatenate an ANSI character after converting it to SAChar
    const SAString &operator+=(char ch);
#endif
    // concatenate from a SAChar string
    const SAString &operator+=(const SAChar *lpsz);

    friend SAString SQLAPI_API operator+(const SAString &string1, const SAString &string2);
    friend SAString SQLAPI_API operator+(const SAString &string, SAChar ch);
    friend SAString SQLAPI_API operator+(SAChar ch, const SAString &string);
#ifdef SA_UNICODE
    friend SAString SQLAPI_API operator+(const SAString &string, char ch);
    friend SAString SQLAPI_API operator+(char ch, const SAString &string);
#endif
    friend SAString SQLAPI_API operator+(const SAString &string, const SAChar *lpsz);
    friend SAString SQLAPI_API operator+(const SAChar *lpsz, const SAString &string);

    // string comparison

    // straight character comparison
    int Compare(const SAChar *lpsz) const;
    // compare ignoring case
    int CompareNoCase(const SAChar *lpsz) const;
    // NLS aware comparison, case sensitive
    int Collate(const SAChar *lpsz) const;

    // simple sub-string extraction

    // return all characters starting at zero-based nFirst
    SAString Mid(int nFirst) const;
    // return nCount characters starting at zero-based nFirst
    SAString Mid(int nFirst, int nCount) const;
    // return first nCount characters in string
    SAString Left(int nCount) const;
    // return nCount characters from end of string
    SAString Right(int nCount) const;

    // trimming whitespace (either side)

    // remove whitespace starting from right edge
    void TrimRight();
    // remove whitespace starting from left side
    void TrimLeft();

    // trimming anything (either side)

    // remove continuous occurrences of chTarget starting from right
    void TrimRight(SAChar chTarget);
    // remove continuous occcurrences of characters in passed string,
    // starting from right
    void TrimRight(const SAChar *lpszTargets);
    // remove continuous occurrences of chTarget starting from left
    void TrimLeft(SAChar chTarget);
    // remove continuous occcurrences of characters in
    // passed string, starting from left
    void TrimLeft(const SAChar *lpszTargets);

    // advanced manipulation

    // replace occurrences of substring lpszOld with lpszNew;
    // empty lpszNew removes instances of lpszOld
    int Replace(const SAChar *lpszOld, const SAChar *lpszNew);
    // insert character at zero-based index; concatenates
    // if index is past end of string
    int Insert(int nIndex, SAChar ch);
    // insert substring at zero-based index; concatenates
    // if index is past end of string
    int Insert(int nIndex, const SAChar *pstr);
    // delete nCount characters starting at zero-based index
    int Delete(int nIndex, int nCount = 1);

    // searching

    // find character starting at left, -1 if not found
    int Find(SAChar ch) const;
    // find character starting at right
    int ReverseFind(SAChar ch) const;
    // find character starting at zero-based index and going right
    int Find(SAChar ch, int nStart) const;
    // find first instance of any character in passed string
    int FindOneOf(const SAChar *lpszCharSet) const;
    // find first instance of substring
    int Find(const SAChar *lpszSub) const;
    // find first instance of substring starting at zero-based index
    int Find(const SAChar *lpszSub, int nStart) const;

    // simple formatting

    // printf-like formatting using passed string
	//!!!!Format has a bug in this version ,do not use it, use standard c/c++ function replace it!!!
    void Format(const SAChar *lpszFormat, ...);
    // printf-like formatting using variable arguments parameter
    void FormatV(const SAChar *, va_list argList);

    // Access to string implementation buffer as "C" character array

    // get pointer to modifiable buffer at least as long as nMinBufLength
    SAChar *GetBuffer(int nMinBufLength);
    // release buffer, setting length to nNewLength (or to first nul if -1)
    void ReleaseBuffer(int nNewLength = -1);

    // Use LockBuffer/UnlockBuffer to turn refcounting off

    // turn refcounting off
    SAChar *LockBuffer();
    // turn refcounting back on
    void UnlockBuffer();

    // get binary data length (in bytes)
    int GetBinaryLength() const;
    // return pointer to const binary data buffer
    operator const void *() const;
    // get pointer to modifiable binary data buffer at least as long as nMinBufLengthInBytes
    void *GetBinaryBuffer(int nMinBufLengthInBytes);
    // release buffer, setting length to nNewLength (or to first nul if -1)
    void ReleaseBinaryBuffer(int nNewLengthInBytes);

// Special conversion functions (multibyte <-> Unicode)
#ifdef SA_UNICODE
    // return pointer to const multibyte string, convert if needed
    const char *GetMultiByteChars() const;
    // get string length (in multibyte characters)
    int GetMultiByteCharsLength() const;
    #define GetWideChars operator const SAChar *
    #define GetWideCharsLength GetLength
#else   // !SA_UNICODE
    #define GetMultiByteChars operator const SAChar *
    #define GetMultiByteCharsLength GetLength
    // return pointer to const Unicode string, convert if needed
    const wchar_t *GetWideChars() const;
    int GetWideCharsLength() const;
#endif  // !SA_UNICODE

    void ExtendBuffer(int nNewLen);


// Implementation
public:
    ~SAString();

protected:
    SAChar *m_pchData;  // pointer to ref counted string data

    // implementation helpers
    //SAStringData *GetData() const;
    inline SAStringData *GetData() const
    {
        assert(m_pchData != NULL);
        return ((SAStringData *)m_pchData)-1;
    }
    void Init();
    void AllocBuffer(int nLen);
#ifdef SA_UNICODE
    void AssignBinaryCopy(int nSrcLenInBytes, const void *pSrcData);
    void ConcatBinaryCopy(int nSrc1LenInBytes, const void *pSrc1Data, int nSrc2LenInBytes, const void *pSrc2Data);
    void ConcatBinaryInPlace(int nSrcLen, const void *pData);
#endif  // SA_UNICODE
    void AssignCopy(int nSrcLen, const SAChar *lpszSrcData);
    void ConcatCopy(int nSrc1Len, const SAChar *lpszSrc1Data, int nSrc2Len, const SAChar *lpszSrc2Data);
    void ConcatInPlace(int nSrcLen, const SAChar *lpszSrcData);

    void CopyBeforeWrite();
    void AllocBeforeWrite(int nLen);
    void Release();
    static void Release(SAStringData *pData);
    static int SafeStrlen(const SAChar *lpsz);
    static void FreeData(SAStringData *pData);

private:
    friend class saStringOptInit;
};
inline bool operator==(const SAString &s1, const SAString &s2)
{
    return s1.Compare(s2) == 0;
}
inline bool operator==(const SAString &s1, const SAChar *s2)
{
    return s1.Compare(s2) == 0;
}
inline bool operator==(const SAChar *s1, const SAString &s2)
{
    return s2.Compare(s1) == 0;
}
inline bool operator!=(const SAString &s1, const SAString &s2)
{
    return s1.Compare(s2) != 0;
}
inline bool operator!=(const SAString &s1, const SAChar *s2)
{
    return s1.Compare(s2) != 0;
}
inline bool operator!=(const SAChar *s1, const SAString &s2)
{
    return s2.Compare(s1) != 0;
}
inline bool operator<(const SAString &s1, const SAString &s2)
{
    return s1.Compare(s2) < 0;
}
inline bool operator<(const SAString &s1, const SAChar *s2)
{
    return s1.Compare(s2) < 0;
}
inline bool operator<(const SAChar *s1, const SAString &s2)
{
    return s2.Compare(s1) > 0;
}
inline bool operator>(const SAString &s1, const SAString &s2)
{
    return s1.Compare(s2) > 0;
}
inline bool operator>(const SAString &s1, const SAChar *s2)
{
    return s1.Compare(s2) > 0;
}
inline bool operator>(const SAChar *s1, const SAString &s2)
{
    return s2.Compare(s1) < 0;
}
inline bool operator<=(const SAString &s1, const SAString &s2)
{
    return s1.Compare(s2) <= 0;
}
inline bool operator<=(const SAString &s1, const SAChar *s2)
{
    return s1.Compare(s2) <= 0;
}
inline bool operator<=(const SAChar *s1, const SAString &s2)
{
    return s2.Compare(s1) >= 0;
}
inline bool operator>=(const SAString &s1, const SAString &s2)
{
    return s1.Compare(s2) >= 0;
}
inline bool operator>=(const SAString &s1, const SAChar *s2)
{
    return s1.Compare(s2) >= 0;
}
inline bool operator>=(const SAChar *s1, const SAString &s2)
{
    return s2.Compare(s1) <= 0;
}

class SQLAPI_API SANull
{
};

#define SA_NUMERIC_MANTISSA_SIZE 53



class SQLAPI_API SAStringMap
{
public:
    SAStringMap(const char * szName, const  char * szValue = "");
    const char* GetName() const;
    char* GetValue();
    virtual ~SAStringMap();
    /*
        onst char* GetValue() const;
       */
    void SetValue(const char * sValue);
};

class SQLAPI_API SAConnection
{
    friend class SACommand;
    friend class SAField;
private:
    // disable copy constructor
    SAConnection(const SAConnection &);
    // disable assignment operator
    SAConnection &operator = (const SAConnection &);
    int nReserved;
public:
    SAConnection();
	//virtual ~SAConnection();
	inline ~SAConnection()
	{
	};
    bool isConnected() const;
    void Connect(const SAString &sDBString,const SAString &sUserID,const SAString &sPassword );
    void Connect(const SAString &sDBString, void* henv = NULL, void* hdbcsvc = NULL);

    void Disconnect();
    void Commit();
};

class SQLAPI_API SACommand
{
private:
    // disable copy constructor
    SACommand(const SACommand &);
    // disable assignment operator
    SACommand &operator = (const SACommand &);

    SAConnection    *m_pConnection;
    SAString        m_sCmd;
    bool            m_bExecuted;

public:
	void Execute(int nRowCount);

    SACommand();    // construct command with no associated connection and SQL
    SACommand(      // construct command based on the given connection and SQL
		SAConnection *pConnection,
		const SAString &sCmd = SAString());

    //virtual ~SACommand();
    inline virtual ~SACommand() 
    {
    };
    SAConnection *Connection() const;
    void setConnection(SAConnection *pConnection);

    virtual void Open();
    virtual bool isOpened();
    virtual void Close();
    void setCommandText(
        const SAString &sSQL,
        SACommandType_t eCmdType = SA_CmdUnknown);
    //SAString CommandText() const;
    const SAString &CommandText() const;
};





class SQLAPI_API SAException
{
public:
	SAException();
    virtual ~SAException();
    int ErrPos() const;
    SAString ErrText() const;

protected:
    SAString        m_sMsg;
};


#endif // !defined(_SQLAPI_H__INCLUDED_)
