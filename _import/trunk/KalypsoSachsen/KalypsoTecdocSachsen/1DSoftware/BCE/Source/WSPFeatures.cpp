// WSPFeatures.cpp: Implementierung der Klasse WSPFeatures.
//
//////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#pragma warning(disable:4503)


#include "..\include\WSPFeatures.h"

// statische Variablen initialisieren ---------------------------------------------

TCHAR* WSPFeatures::m_TheKey = "Bjoernsen Beratende Ingenieure GmbH";	// der Schlüssel für die Verschlüsselung der Registry
TCHAR* WSPFeatures::m_RegPath = "Software\\BCE";						//der Pfad im Registryzweig
HKEY WSPFeatures::m_HKEY = HKEY_LOCAL_MACHINE;							//Zweig der Registry
TCHAR* WSPFeatures::m_RegKey = "key";									//zu diesem Schlüssel

WSPFeatures WSPFeatures::m_instance;									//Instanz erzeugen
// statische Variablen initialisieren ---------------------------------------------



//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////
Feature::Feature(const TCHAR* StringData, const bool enabled )
{
	isEnabled = enabled;
	StrData = StringData;
}

WSPFeatures::WSPFeatures(void)
{
// beim erzeugen direkt die Registry auslesen,
// geht auch später noch  mit einem anderen Registryzweig zu überschreiben.
//						 public ReadRegistryKey -> leert die Map und liest neue Werte ein
	ReadRegistryKey();// private ReadRegistryKey arbeitet mit den statischen Werten
					// kann die Registry nicht gelesen werden, wird die Sektion HEAD erzeugt	
}

WSPFeatures::~WSPFeatures()
{
  ClearFeatures();// features* freigeben
}

void WSPFeatures::ClearFeatures()
{
for(SectionsIterator it = begin(); it != end(); it++ )
  {
	FeatureMap* fm = it->second;
	for (FeatureMapIterator fmit = fm->begin(); fmit != fm->end(); fmit++)
	{
		delete fmit->second;    //delete Feature
	}
	fm->clear();
	delete it->second; // delete FeatureMap
  }
  this->clear(); // clear Sections
}

void WSPFeatures::CodeKey(TCHAR* EncodedKeyBuffer,long BufferSize)
{
	long KeySize = _tcslen(m_TheKey);
  for(long BPos = 0; BPos < BufferSize; ++BPos )
      EncodedKeyBuffer[BPos] ^= m_TheKey[BPos % KeySize];
}

void WSPFeatures::ReadRegistryKey(void)	// private
{
	m_RegLoaded = (ReadRegistryKey(m_HKEY,m_RegPath,m_RegKey)==ERROR_SUCCESS);
	if (!m_RegLoaded)
	{
		if(AddFeature("HEAD","LICENCE_TEXT1"))
			SetDataStr("HEAD","LICENCE_TEXT1","Demo Version");
		if(AddFeature("HEAD","DEMO_INFO"))
			WSPFeatures::Instance()->SetDataStr("HEAD","DEMO_INFO","Ihre Lizenzeinstellungen erlauben es Ihnen nicht diese Funktion zu nutzen.");
	}
}


long WSPFeatures::ReadRegistryKey(HKEY RegHKEY,const TCHAR* RegPath,const TCHAR* RegKey)
{
	HKEY OpenKey = 0;
	DWORD StrSize = 0;
	DWORD TheValueType = REG_NONE;
	long Result = ERROR_INVALID_PARAMETER;
	TCHAR* DecodedKeyBuffer;


	Result = RegOpenKeyEx (RegHKEY,RegPath,0,KEY_READ,&OpenKey);				//Handle auf den RegPfad erzeugen
	if (Result == ERROR_SUCCESS)
	{
		Result = RegQueryValueEx(OpenKey,RegKey,0,&TheValueType,NULL,&StrSize); //Länge des RegEintrags in Byte ermitteln
		if ((Result == ERROR_SUCCESS) && (TheValueType == REG_BINARY))			//Schlüssel ist offen und der Eintrag ist binär
		{
			ClearFeatures();													//Map leeren
			DecodedKeyBuffer = (TCHAR*)malloc(StrSize);							//Speicher für DecodedKeyBuffer auf dem HEAP reservieren
            Result = RegQueryValueEx(OpenKey,RegKey,0,NULL,
											(LPBYTE)DecodedKeyBuffer,&StrSize); //Schlüssel nach DecodedKeyBuffer kopieren
			long StrLen = *(long*)DecodedKeyBuffer;								//Zeichenanzahl auslesen(die ersten 4Bytes sind immer uncodiert)
			bool decoded = (StrLen < 0);										//-> negativer Wert = codierter Eintrag
			StrLen = abs(StrLen);
			if ((StrSize-5) % StrLen == 0)										//Checksumme(Anzahl der Bytes - 4ByteLong - abschliessendem NullByte)
			{
				if (decoded) CodeKey(DecodedKeyBuffer+4,StrLen);				//DecodedKeyBuffer decodieren
				BuildFeatureMap(DecodedKeyBuffer+4,StrLen);						//Die Map erzeugen
				free(DecodedKeyBuffer);												
			}
			else Result = ERROR_INVALID_PARAMETER;
		}
		RegCloseKey(OpenKey);													// Registry schließen
	}
	return Result;
}

bool WSPFeatures::WriteRegFile(const TCHAR* RegFileName,byte *EncodedKeyBuffer,long BufferSize)
{
	std::ofstream TheFile( RegFileName,std::ios::out );							//erzeugt eine Windows *.reg Datei
	if( TheFile.is_open() )
	{
		//	TheFile << "Windows Registry Editor Version 5.00" << std::endl;			//Kopfdaten schreiben
		// damit auch NT4 mit den .reg umgehen können, besser nach REGEDIT4 standard schreiben
		TheFile << "REGEDIT4" << std::endl;			//Kopfdaten schreiben
		if (m_HKEY == HKEY_CURRENT_USER)  TheFile << "[HKEY_CURRENT_USER\\";
		if (m_HKEY == HKEY_LOCAL_MACHINE) TheFile << "[HKEY_LOCAL_MACHINE\\";
		TheFile << m_RegPath << "]" << std::endl << "\"" << m_RegKey << "\"=hex:";
	}
	else 
		return false;
	
	for (byte* b = (byte*)&BufferSize;b < (byte*)&BufferSize+4;b++)			//Zeichenanzahl schreiben
	{
		TheFile << std::hex <<  (unsigned int)*b << ',';
	}
	
	unsigned int i = 0;
	for(long BufferPos = 0;BufferPos < abs(BufferSize);BufferPos++)			//EncodedKeyBuffer Byteweise schreiben
	{
		i = EncodedKeyBuffer[BufferPos];
		TheFile << std::hex << i << ',';
	}
	TheFile << 0;															//abschließendes Nullbyte (immer uncodiert)
	TheFile << std::endl; // benötigt von REGEDIT4, sonst wird nichts eingetragen!
	TheFile.close();
	return true;
}


bool WSPFeatures::IniToReg(const TCHAR* aIniFile, const TCHAR* aRegFile, bool decodeKey)
{

//------------------grösser als das komplette Ini-File kann der benötigte Speicherplatz nicht werden
	HANDLE HFile = CreateFile (aIniFile,GENERIC_READ,0,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
	if (HFile == INVALID_HANDLE_VALUE) return false;

	long MaxBufferSize = GetFileSize(HFile,NULL);// Grösse der Ini-Datei in bytes
	CloseHandle(HFile);

	TCHAR* EncodedKeyBuffer= (TCHAR*)malloc(MaxBufferSize);
	long EncodedKeyStrLen = 0;
//----------------------------------

	TCHAR* SectionBuffer = (TCHAR*)malloc(1024); // reicht für etwa 50 SektionsNamen
	TCHAR* pSection = SectionBuffer;

	int SectionStrLen = GetPrivateProfileString(NULL,NULL,"",SectionBuffer,1024,aIniFile); // SektionsNamen einlesen
	for (int i = 0;i < SectionStrLen;++i)
	{
		if (SectionBuffer[i] == 0)
		{
			EncodedKeyBuffer[EncodedKeyStrLen] = 0;	//Schlüssel-Wert-Paare der Sektion auslesen
			EncodedKeyStrLen++;
			int KeyStrLen = GetPrivateProfileSection(pSection,&EncodedKeyBuffer[EncodedKeyStrLen],MaxBufferSize-EncodedKeyStrLen, aIniFile);//Key=Value Paare für die Sektion in Buffer schreiben
			EncodedKeyStrLen = EncodedKeyStrLen + KeyStrLen;

			EncodedKeyBuffer[EncodedKeyStrLen] = 0;
			EncodedKeyStrLen++;

			pSection =&SectionBuffer[i+1];			//pointer auf nächste Sektion setzen
			
		}
		else
		{
			EncodedKeyBuffer[EncodedKeyStrLen] = SectionBuffer[i];// Sektionsnamen Zeichenweise kopieren
			EncodedKeyStrLen++;
		}

	}
	EncodedKeyBuffer[EncodedKeyStrLen] = 0;
	EncodedKeyStrLen++;
	if (decodeKey) 
	{
		CodeKey(EncodedKeyBuffer,EncodedKeyStrLen);				// Buffer codieren
		EncodedKeyStrLen = -EncodedKeyStrLen;					// -Zeichennzahl bedeutet, daß der Buffer codiert ist
	}
	bool result = WriteRegFile(aRegFile,(byte*)EncodedKeyBuffer,EncodedKeyStrLen);// *.reg File erstellen
	free(SectionBuffer);
	free(EncodedKeyBuffer);
	return result;
}

void WSPFeatures::BuildFeatureMap(TCHAR* EncodedKeyBuffer, long StrSize)
{	
	/*
		EncodedKeyBuffer hat die Form
		Section1 0x0  Key1=Value 0x0 Key2=Value 0x0 Key3=.... 0x0 0x0 Section2 0x0 Key=....0x0 0x0 0x0
	
		StrSize ist die Anzahl der Zeichen (nicht die Länge in Bytes) inclusive der abschließenden Nullwerte
	*/
	TCHAR* pSection = EncodedKeyBuffer; // Zeiger auf die aktuelle Sektion
	long EncodedKeyPos = 0;				// Position auf dem Buffer(kein Adresszeiger sondern Anzahl der Zeichen)
	bool FeatureIsOn = true;					
	FeatureMap* fs;						// aktuelle (Sektions)Map 

	while ((*pSection != 0) && (EncodedKeyPos < StrSize))
	{
		AddSection(pSection,fs);		//Sektion mit diesem Name erzeugen, oder bestehende holen
		EncodedKeyPos = EncodedKeyPos + _tcslen(pSection) +1;
		TCHAR* pKey = &EncodedKeyBuffer[EncodedKeyPos];
		while((*pKey != 0) && (EncodedKeyPos < StrSize))	//Key=Value-Paare in die (Sektions)Map einlesen
		{
			if (*pKey == '!')
			{
				EncodedKeyPos++;
				pKey = &EncodedKeyBuffer[EncodedKeyPos];
				FeatureIsOn = false;
			}
			while (EncodedKeyBuffer[EncodedKeyPos] != '=') // '=' von Key=Value suchen 
			{
				if  (EncodedKeyPos < StrSize) 
					EncodedKeyPos++;
				else break;
			}
			if  (EncodedKeyPos < StrSize)
				EncodedKeyBuffer[EncodedKeyPos] = 0; // erzeugt aus Key=Value -> Key 0x0 Value
				else break;
			EncodedKeyPos++;
			TCHAR* pValue = &EncodedKeyBuffer[EncodedKeyPos];
			
			AddFeature(fs,pKey);					// Neues Feature in der (Sektions)Map erzeugen, oder bestehendes holen
			if (FeatureIsOn) EnableFeature(pSection,pKey);
			SetDataStr(pSection,pKey,pValue);

			EncodedKeyPos = EncodedKeyPos + _tcslen(pValue)+1;// pointer nachführen
			pKey = &EncodedKeyBuffer[EncodedKeyPos];
			FeatureIsOn = true;
		}
		EncodedKeyPos++;
		pSection = &EncodedKeyBuffer[EncodedKeyPos];		 // pointer nachführen
	}
}

FeatureMap* WSPFeatures::GetSection(const TCHAR* SectionName)
{
	SectionsIterator sit = this->find(SectionName);
	if (sit != this->end())
		return sit->second;
	else
		return NULL;
}

bool WSPFeatures::GetSection(const TCHAR* SectionName, FeatureMap*& SectionMap)
{
	SectionMap = GetSection(SectionName);
	return (SectionMap != NULL);
}

Feature* WSPFeatures::GetFeature(const TCHAR* SectionName, const TCHAR* FeatureName)
{
	FeatureMap* fm;
	if (GetSection(SectionName, fm))
	{
		FeatureMapIterator fmit = fm->find(FeatureName);
		if (fmit != fm->end())
			return fmit->second;
		else return NULL;

	}
	else return NULL;
}

bool WSPFeatures::GetFeature(const TCHAR* SectionName, const TCHAR* KeyName, Feature*& TheFeature)
{
	TheFeature = GetFeature(SectionName,KeyName);
	return (TheFeature != NULL);
}

const TCHAR* WSPFeatures::GetDataStr(const TCHAR* SectionName, const TCHAR* KeyName)
{
	Feature* f;
	if (GetFeature(SectionName, KeyName, f))
		return f->StrData.c_str();
	else return "";
}

bool WSPFeatures::GetDataStr(const TCHAR* SectionName, const TCHAR* KeyName, const TCHAR*& DataStr)
{
	Feature* f;
	if (GetFeature(SectionName, KeyName, f))
	{
		DataStr = f->StrData.c_str();
		return true;
	}
	else
	{
		return false;
	}
}

bool WSPFeatures::isEnabled(const TCHAR* SectionName, const TCHAR* KeyName)
{
	Feature* f;
	if (!GetFeature(SectionName, KeyName, f)) return false;
	return f->isEnabled;
}

bool WSPFeatures::AddSection(const TCHAR* SectionName, FeatureMap*& SectionMap)
{
	this->insert(Sections::value_type(SectionName,new FeatureMap));
	return GetSection(SectionName,SectionMap);
}

bool WSPFeatures::AddFeature(FeatureMap* SectionMap, const TCHAR* FeatureName)
{
	FeatureMapIterator fit = SectionMap->find(FeatureName);
	if (fit != SectionMap->end()) return true;
	SectionMap->insert(FeatureMap::value_type(FeatureName,new Feature("",false)));
	fit = SectionMap->find(FeatureName);
	return (fit != SectionMap->end());
}

bool WSPFeatures::AddFeature(const TCHAR* SectionName, const TCHAR* FeatureName)
{
	FeatureMap* fm;
	if (GetSection(SectionName,fm))	return AddFeature(fm, FeatureName);
	if (AddSection(SectionName,fm))	return AddFeature(fm, FeatureName);
	return false;
}

bool WSPFeatures::EnableFeature(const TCHAR* SectionName, const const TCHAR* KeyName)
{
 Feature* f;
 if (GetFeature(SectionName, KeyName, f))
 {
	f->isEnabled = true;
	return true;
 }
 else return false;
}

bool WSPFeatures::DisableFeature(const TCHAR* SectionName, const const TCHAR* KeyName)
{
 Feature* f;
 if (GetFeature(SectionName, KeyName, f))
 {
	f->isEnabled = false;
	return true;
 }
 else return false;
}

bool WSPFeatures::SetDataStr(const TCHAR* SectionName, const TCHAR* KeyName,const TCHAR* DataString)
{
 Feature* f;
 if (GetFeature(SectionName, KeyName, f))
 {
	f->StrData = DataString; 
	return true;
 }
 else return false;
}

/**
* Testet, ob das angegebene Datum vor oder nach heute liegt. Ist der übergebene String leer, wird immer true zurückgegeben.
*/
bool WSPFeatures::CheckDemoDate(const TCHAR* aDate)
{
	if( _tcslen( aDate ) == 0 )
		return true;

  int month, year, day;
  _stscanf(aDate, "%d.%d.%d", &day, &month, &year );
  tm datum;
  time_t heute,_datum;

  time(&heute);
  
  datum.tm_hour = 0;
  datum.tm_isdst = 0;
  datum.tm_mday = day;
  datum.tm_min = 0;
  datum.tm_mon = month - 1;
  datum.tm_sec = 0;
  datum.tm_wday = -1;
  datum.tm_yday = -1;
  datum.tm_year = year - 1900;
  _datum = mktime( &datum );
  return (((long)_datum > 0) && (long)_datum > (long)heute);
}
/**
 * Vergleicht die beiden gegebenen Versionsstring zeichenweise. 'x' seitens version ist
 * wildcard.
 */
bool WSPFeatures::CheckRegVer(const TCHAR* version,const TCHAR* ver_exe)
{
	int vl = _tcslen( ver_exe );
	if( vl != _tcslen( version ) )
		return false;

	for( int i = 0;i < vl;i++ )
	{
		if( ( version[i] != 'x' ) && ( ver_exe[i] != version[i] ) )
			return false;
	}
	return true;
}