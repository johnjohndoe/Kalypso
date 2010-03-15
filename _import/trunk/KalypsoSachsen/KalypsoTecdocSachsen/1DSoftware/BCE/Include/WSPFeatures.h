// WSPFeatures.h: Schnittstelle für die Klasse WSPFeatures.
//
//////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#if !defined(WSP_FEATURES_H__INCLUDED_)
#define WSP_FEATURES_H__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


#include <string>
#include <map>
#include <tchar.h>
#include <windows.h>
#include <fstream>
#include <time.h>


class Feature
{
	public:
		Feature(const TCHAR* StringData, const bool enabled);
		std::string StrData;
		bool isEnabled;
}; // class Feature


typedef std::map<std::string,Feature* > FeatureMap;
typedef	FeatureMap::iterator FeatureMapIterator;

typedef std::map<std::string,FeatureMap* > Sections;
typedef	Sections::iterator SectionsIterator;


class WSPFeatures : Sections
{
private:
//----------------------------------------------------------------------------------
	static TCHAR* m_TheKey;
	static TCHAR* m_RegPath;
	static TCHAR* m_RegKey;
	static HKEY m_HKEY;
	static WSPFeatures m_instance;
	bool m_RegLoaded;
	WSPFeatures(void);
//----------------------------------------------------------------------------------
	void BuildFeatureMap(TCHAR* EncodedKeyBuffer, long StrSize);
	bool WriteRegFile(const TCHAR* RegFileName,byte *EncodedKeyBuffer,long BufferSize);
	void CodeKey(TCHAR* EncodedKeyBuffer, long BufferSize);
	void ReadRegistryKey(void);
	void ClearFeatures(void);

public:

	virtual ~WSPFeatures();
	static WSPFeatures* Instance() { return &m_instance; };

	bool isEmpty(void){return this->empty();};
	bool RegLoaded(void){return this->m_RegLoaded;};

	bool IniToReg(const TCHAR* aIniFile,const TCHAR* aRegFile, bool decodeKey);
	long ReadRegistryKey(HKEY RegHKEY,const TCHAR* RegPath,const TCHAR* RegKey);
	
	bool AddSection(const TCHAR* SectionName, FeatureMap*& SectionMap);
	FeatureMap* GetSection(const TCHAR* SectionName);
	bool GetSection(const TCHAR* SectionName, FeatureMap*& SectionMap);

	bool GetFeature(const TCHAR* SectionName, const TCHAR* FeatureName, Feature*& TheFeature);
	Feature* GetFeature(const TCHAR* SectionName, const TCHAR* FeatureName);
	bool AddFeature(const TCHAR* SectionName, const TCHAR* FeatureName);
	bool AddFeature(FeatureMap* SectionMap, const TCHAR* FeatureName);

	const TCHAR* GetDataStr(const TCHAR* SectionName, const TCHAR* KeyName);
	bool GetDataStr(const TCHAR* SectionName, const TCHAR* KeyName, const TCHAR*& DataStr);
	bool isEnabled (const TCHAR* SectionName, const TCHAR* KeyName);

	bool SetDataStr(const TCHAR* SectionName, const TCHAR* KeyName,const const TCHAR* DataString);
	bool EnableFeature(const TCHAR* SectionName, const const TCHAR* KeyName);
	bool DisableFeature(const TCHAR* SectionName, const const TCHAR* KeyName);

	bool CheckDemoDate(const TCHAR* aDate);
	bool CheckRegVer(const TCHAR* version, const TCHAR* ver_exe);

};// class WSPFeatures

//------------functions-------------------------------



#endif // !defined(WSP_FEATURES_H__INCLUDED_)
