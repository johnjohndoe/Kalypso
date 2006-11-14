package org.kalypso.model.km;

import java.io.File;

public class ProfileData
{
  private final double m_meter;

  private ProfileData m_nextProfile;

  private ProfileData m_prevProfile;

  private final double m_min;

  private final double m_max;

  private Row[] m_rows;

  private final File m_file;

  public ProfileData( final File file, double min, double max, double meter )
  {
    m_file = file;
    m_meter = meter;
    m_min = min;
    m_max = max;
  }

  public File getFile( )
  {
    return m_file;
  }

  /**
   * @return
   */
  public double getRange( double minPos, double maxPos )
  {
    final double resultMax;
    final double resultMin;

    if( m_nextProfile == null )
      resultMax = maxPos;
    else
      resultMax = Math.min( getPosition() + (m_nextProfile.getPosition() - getPosition()) / 2d, maxPos );
    if( m_prevProfile == null )
      resultMin = minPos;
    else
      resultMin = Math.max( getPosition() - (getPosition() - m_prevProfile.getPosition()) / 2d, minPos );
    // if( resultMax - resultMin < 0 )
    // System.out.println( " debug" );
    return resultMax - resultMin;
  }

  public String toString( )
  {
    final StringBuffer result = new StringBuffer( "Profile " + m_meter + "[km]\n" );
    for( int i = 0; i < m_rows.length; i++ )
    {
      Row row = m_rows[i];

      result.append( row.toString() );
      result.append( "\n" );

    }
    return result.toString();
  }

  public double getPosition( )
  {
    return m_meter;
  }

  public void setPrev( ProfileData profile )
  {
    m_prevProfile = profile;
  }

  public void setNext( ProfileData profile )
  {
    m_nextProfile = profile;
  }

  public void set( Row[] rowArray )
  {
    m_rows = rowArray;
  }

  public int getNumberKMValues( )
  {
    return m_rows.length - 1;
  }

  public AbstractKMValue getKMValue( int index )
  {
    return new KMValue( getRange( m_min, m_max ), m_rows[index], m_rows[index + 1] );
  }

  public boolean isValidForKalypso( )
  {
    if( m_rows.length < 6 ) // not enough values for calculation
    {
      System.out.println( ", Profildaten nicht O.K.! Es liegen nicht genug Abflüsse zur Berechnung vor (mind. 6). \n" );
      return false;
    }
    if( m_rows[0].getAlpha() < 1 ) // q is allways > q-bordfull
    {
      System.out.println( ", Profildaten nicht O.K.! Keine Abflüsse unter bordvollem Abfluss vorhanden. \n" );
      return false;
    }
    if( m_rows[2].getAlpha() < 1 ) // no enough values below bordfull (Mindestens die ersten 3 Werte müssen!)
    {
      System.out.println( ", Profildaten nicht O.K.! Nicht genug Abflüsse unter bordvollem Abfluss vorhanden (Die ersten 3 Werte müssen unter dem bordvollem Abfluss liegen). \n" );
      return false;
    }

    // TODO: Check if values above bordfull is necessary!
    // if( m_rows[m_rows.length - 1].getAlpha() >= 1.0 ) // no values above bordfull
    // {
    // System.out.println( ", Profildaten nicht O.K.! Keine Abflüsse über bordvollem Abfluss vorhanden. \n" );
    // return false;
    // }
    // if( m_rows[m_rows.length - 1 - 2].getAlpha() >= 1.0 ) // no enough values above bordfull (Mindestens die letzten
    // 3
    // // Werte müssen!)
    // {
    // System.out.println( ", Profildaten nicht O.K.! Nicht genug Abflüsse über bordvollem Abfluss vorhanden (Die
    // letzten 3 Werte müssen über dem bordvollem Abfluss liegen). \n" );
    // return false;
    // }
    System.out.println( ", Profildaten sind O.K.. \n" );
    return true;

  }
}