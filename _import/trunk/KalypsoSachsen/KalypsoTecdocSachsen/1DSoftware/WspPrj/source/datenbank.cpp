#include "stdafx.h"

#include "giostr.h"

#include "datenbank.h"

  ////////////////////////////
  //  Klasse  Datenbank
  ///////////////////////////

/* The Default Constructor */
Datenbank::Datenbank(int type)
{
	m_nType = (Type)type;	
}

Datenbank::~Datenbank()
{
}

int Datenbank::GetType()
{
	return m_nType;	
}

void Datenbank::SetType(int type)
{
	m_nType = (Type)type;
}

void Datenbank::GetName(int i, CString& name)
{
	name = m_names[i];
}

void Datenbank::SetName(int i, CString& name)
{
	m_names[i] = name;
}

double Datenbank::GetVar1(int i)
{
	return m_vars1[i];
}

void Datenbank::SetVar1(int i, double value)
{
	m_vars1[i] = value;
}

double Datenbank::GetVar2(int i)
{
	return m_vars2[i];
}

void Datenbank::SetVar2(int i, double value)
{
	m_vars2[i] = value;
}

double Datenbank::GetVar3(int i)
{
	return m_vars3[i];
}

void Datenbank::SetVar3(int i, double value)
{
	m_vars3[i] = value;
}

CString Datenbank::GetDatabankName()
{
	return m_DatabankName;
}

void Datenbank::SetDatabankName(CString databankname)
{
	m_DatabankName = databankname;
}

BOOL Datenbank::Load()
{
	CFileStatus rStatus;
	gifstream ifs;
	CString rString;
	if (CFile::GetStatus(m_DatabankName, rStatus))
	{
		ifs.open(m_DatabankName, ios::in);
		if (ifs.fail())
		{
			rString.FormatMessage("Konnte Datei %1 nicht zum Lesen öffnen.", m_DatabankName);
			AfxMessageBox(rString, MB_ERROR);
			return FALSE;
		}
		else
		{
			ifs >> *this;
			ifs.close();
		}
		
	}
	return TRUE;
}

BOOL Datenbank::Save()
{
	CFileStatus rStatus;
	gofstream ofs;
	CString rString;
	//if (CFile::GetStatus(m_DatabankName, rStatus))
	//{
		ofs.open(m_DatabankName, ios::out | ios::trunc);
		if (ofs.fail())
		{
			rString.FormatMessage("Konnte Datei %1 nicht zum Schreiben öffnen.", m_DatabankName);
			AfxMessageBox(rString, MB_ERROR);
			return FALSE;
		}
		else
		{
			ofs << *this;
			ofs.close();
		}
		
	//}
	return TRUE;
}

int Datenbank::GetSize()
{
	return m_names.GetSize();
}

void Datenbank::AddName(CString name)
{
    switch(m_nType)
	    {
	    case ueberfallbeiwert:
        case rauheit_ks:
        case rauheit_kst:
		    {
	         m_names.Add(name);
	         m_vars1.Add(0.0);
            }
            break;
        case bewuchs:
		    {
	         m_names.Add(name);
	         m_vars1.Add(0.0);
             m_vars2.Add(0.0);
             m_vars3.Add(0.0);
            }
            break;
        }
}

istream& operator>>(istream& is, Datenbank &dat)
{
	switch(dat.m_nType)
	{
	case dat.ueberfallbeiwert:
		{
			char buffer[LINE_SIZE];
			CString str;
			while(!is.eof())
			{
                is.getline(buffer, LINE_SIZE, '\n');
                str = buffer;
                str.TrimLeft();
                str.TrimRight();
                if(str.GetLength()==0)
                    break;
                dat.m_names.Add(str);
                if(!is.eof())
				{
                    is.getline(buffer, LINE_SIZE, '\n');
                    dat.m_vars1.Add(atof(buffer));
				}
                else
				{
					dat.m_vars1.Add(0.0);
				}
			}
		}
		break;
    case dat.rauheit_ks:
    case dat.rauheit_kst:
        {
         char buffer[LINE_SIZE];
         CString name;
         char str[LINE_SIZE];
         double wert=0.0;
         while(!is.eof())
             {
             is.getline(buffer, LINE_SIZE, '\n');
             
             if(sscanf(buffer,"%40c%lf",str,&wert)<0)
                 continue;
             name=buffer;
             name=name.Left(40);
             if(name.GetLength()==0)
                 name.Format("Leer");
             dat.m_names.Add(name);             
             dat.m_vars1.Add(wert);              
             }
        }
        break;
     case dat.bewuchs:
        {
         char buffer[LINE_SIZE];
         CString name;
         char str[LINE_SIZE];
         double ax=0.0,ay=0.0,dp=0.0;
         while(!is.eof())
             {
             is.getline(buffer, LINE_SIZE, '\n');
             
             if(sscanf(buffer,"%40c%lf%lf%lf",str,&ax,&ay,&dp)<0)
                 continue;
             name=buffer;
             name=name.Left(40);
             if(name.GetLength()==0)
                 name.Format("Leer");
             dat.m_names.Add(name);             
             dat.m_vars1.Add(ax);
             dat.m_vars2.Add(ay);
             dat.m_vars3.Add(dp);
             }
        }
        break;
	}
	return is;
}

ostream& operator<<(ostream& os, Datenbank &dat)
{
	int i,num;
	CString str;

	switch(dat.m_nType)
	{
	case dat.ueberfallbeiwert:
		{          
			for(i=0; i<dat.m_names.GetSize(); i++)
			{
				os << dat.m_names[i] << endl;
				str.Format("%.4lf",dat.m_vars1[i]);
				os << str << endl;
			}
		}
		break;
    case dat.rauheit_ks:
    case dat.rauheit_kst:
        {          
			for(i=0; i<dat.m_names.GetSize(); i++)
			{
                dat.m_names[i].TrimLeft();
                dat.m_names[i].TrimRight();
                if(dat.m_names[i].GetLength()>40)
                    dat.m_names[i].SetAt(40,'\0' );
                if((num=dat.m_names[i].GetLength())<=0)
                    continue;
                if(num<40)
                    for(int j=0;j< 40-num;j++)
                         dat.m_names[i]+=' '; 
                    
                str.Format("%s",dat.m_names[i]);
				os << str;
				str.Format(" %.4lf",dat.m_vars1[i]);
				os << str << endl;
			}
		}
		break;
    case dat.bewuchs:
		{          
			for(i=0; i<dat.m_names.GetSize(); i++)
			{
                
                dat.m_names[i].TrimLeft();
                dat.m_names[i].TrimRight();
                if(dat.m_names[i].GetLength()>40)
                    dat.m_names[i].SetAt(40,'\0' );
                if((num=dat.m_names[i].GetLength())<=0)
                    continue;
                if(num<40)
                    for(int j=0;j< 40-num;j++)
                         dat.m_names[i]+=' ';                    
                str.Format("%s",dat.m_names[i]);
				os << str;
				str.Format(" %.4lf",dat.m_vars1[i]);
				os << str;
                str.Format(" %.4lf",dat.m_vars2[i]);
				os << str;
                str.Format(" %.4lf",dat.m_vars3[i]);
				os << str << endl;
			}
		}
		break;
	}
	return os;	
}

