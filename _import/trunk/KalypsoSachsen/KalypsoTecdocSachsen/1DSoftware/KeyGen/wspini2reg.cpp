// wspini2reg.cpp : Definiert den Einsprungpunkt für die Konsolenanwendung.
//

#include "..\BCE\include\WSPFeatures.h"

int main(int argc, char* argv[])
{

	TCHAR drive[3];
	TCHAR path[1024];
	TCHAR filename[255];
	TCHAR fileextension[9];

	TCHAR SourceFile[1024];
	TCHAR DestinationFile[1024];

	int chrcnt = 0;
	int doCode = 0; //default: reg-file codieren; sonst doCode = 1;

	if ((argc > 1) && (_tcscmp(argv[1],"-x")==0)) doCode = 1; //Kommandozeilenparameter '-x' schaltet das codieren aus

	switch (argc - doCode)
	{
		case 2:// nur Sourcefile als Parameter, generieren eines gültigen Pfades auf Destinationfile
		{
			if (_tfullpath(SourceFile ,argv[1+doCode],1024)==NULL) lstrcpy(SourceFile,argv[1+doCode]);
			_tsplitpath(SourceFile,drive,path,filename,fileextension);
			_tmakepath(DestinationFile,drive,path,filename,"reg");
			break;
		}
		case 3: // alles angegeben
		{
			if (_tfullpath(SourceFile ,argv[1+doCode],1024)==NULL) lstrcpy(SourceFile,argv[1+doCode]);
			if (_tfullpath(DestinationFile ,argv[2+doCode],1024)==NULL) lstrcpy(DestinationFile,argv[2+doCode]);
			break;
		}
		default: // ausgabe einer Hilfe
		{
			_tsplitpath(argv[0],drive,path,filename,fileextension);
			_tprintf(filename,&chrcnt);
			_tprintf(" [-x] <SourceFile> [<DestinationFile>]",chrcnt);
			_tprintf("\n -x: do not decode");
			return 0;
		}
	}

	if (WSPFeatures::Instance()->IniToReg(SourceFile,DestinationFile, (doCode == 0))) // do it
	{
		_tprintf("new *.reg File -> ",&chrcnt);// ausgabe Destinationfile success
		_tprintf(DestinationFile,chrcnt);
	}
	else
	{
		_tprintf("File not found: ",&chrcnt);//Fehlermeldung
		_tprintf(SourceFile,chrcnt);
		_tprintf("\nor access denied: ",&chrcnt);
		_tprintf(DestinationFile,chrcnt);

	}
	return 0;
}
