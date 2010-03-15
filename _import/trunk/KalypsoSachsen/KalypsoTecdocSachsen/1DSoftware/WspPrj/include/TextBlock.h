// TextBlock.h: Schnittstelle für die Klasse TextBlock.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_TEXTBLOCK_H__F6AF3661_03BB_11D8_B46C_00104BB3E525__INCLUDED_)
#define AFX_TEXTBLOCK_H__F6AF3661_03BB_11D8_B46C_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class DataBlock;

/**
 * @class TextBlock
 *
 * Enthält die Daten eines TextDatenblocks für einen Längsschnitt
 */
class TextBlock  
{
public:
  struct TextSatz
  {
    TextSatz() {};
    TextSatz( const int rA, const int rJ, const int tJ, const int sN, const int tG ) : rahmenArt( rA ), rahmenJustierung( rJ ), textJustierung( tJ ), stationsNummer( sN ), textGroesse( tG ) {};
    TextSatz( const TextSatz& ts )
    {
      *this = ts;
    }

    TextSatz& operator=( const TextSatz& ts )
    {
      rahmenArt = ts.rahmenArt;
      rahmenJustierung = ts.rahmenJustierung;
      textJustierung = ts.textJustierung;
      stationsNummer = ts.stationsNummer;
      textGroesse = ts.textGroesse;
      strings.Copy( ts.strings );

      return *this;
    }

    int rahmenArt;
    int rahmenJustierung;
    int textJustierung;
    int stationsNummer;
    int textGroesse;
    CStringArray strings;
  };
  
public:
  TextBlock( DataBlock* bezugsDatensatz, const double hoehenBezugspunkt ) : m_bezugsDatensatz( bezugsDatensatz ), m_hoehenBezugspunkt( hoehenBezugspunkt ) {};

  void AddTextSatz( TextSatz& textSatz )
  {
    m_textSaetze.Add( textSatz );
  }

  int GetTextSatzCount() const
  {
    return m_textSaetze.GetSize();
  }

  TextSatz GetTextSatz( const int count ) const
  {
    return m_textSaetze[count];
  }

  DataBlock* GetBezugsdatensatz() const
  {
    return m_bezugsDatensatz;
  }

  double GetHoehenbezugspunkt() const
  {
    return m_hoehenBezugspunkt;
  }

  void DeleteWithRef( const int refID )
  {
    for( int i = 0; i < m_textSaetze.GetSize(); i++ )
    {
      TextSatz& ts = m_textSaetze[i];
      if( ts.stationsNummer == refID )
      {
        m_textSaetze.RemoveAt( i );
        i--;
      }
      else if( ts.stationsNummer > refID )
        ts.stationsNummer--;
    }
  }
  
private:
  DataBlock* m_bezugsDatensatz;
  double m_hoehenBezugspunkt;
  CArray<TextSatz, TextSatz&> m_textSaetze;
};

#endif // !defined(AFX_TEXTBLOCK_H__F6AF3661_03BB_11D8_B46C_00104BB3E525__INCLUDED_)
