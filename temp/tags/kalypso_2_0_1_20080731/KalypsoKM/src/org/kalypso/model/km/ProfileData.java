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

  private String m_comment;

  public ProfileData( final File file, double min, double max, double meter )
  {
    m_file = file;
    m_meter = meter;
    m_min = min;
    m_max = max;
    m_comment = null;
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

  public boolean isValidForKalypso( StringBuffer buffer )
  {

    if( m_rows.length < 6 ) // not enough values for calculation
    {
      buffer.append( m_meter + ": Profildaten nicht O.K.! Es liegen nicht genug Abflüsse zur Berechnung vor (mind. 6). \n" );
      return false;
    }
    for( int i = 0; i < m_rows.length - 1; i++ )
    {
      if( (m_rows[i].getQ() - m_rows[i + 1].getQ()) > 0.009 )// Änderung in der dritten Nachkommastelle O.K. -
      // Rechenungenauigkeiten im Hydraulikmodell
      {
        buffer.append( m_meter + ": Profildaten nicht O.K.! Es liegt ein Rücksprung in den Abflüssen des Flussschlauches vor. Zeile: " + (i + 2) + "\n" );
        return false;
      }
    }

    // TODO: Check if values above bordfull is necessary!
    if( m_rows[m_rows.length - 1].getAlpha() >= 1.0 ) // no values above bordfull
    {
      buffer.append( m_meter + ": Profildaten nicht O.K.! Keine Abflüsse über bordvollem Abfluss vorhanden. \n" );
      return false;
    }
    for( int i = 0; i < m_rows.length - 1; i++ )
    {
      if( m_rows[i].getSlope() < 0.0 )
      {
        buffer.append( m_meter + ": Profildaten nicht vollständig O.K.! Es liegt ein negatives Spiegelliniengefälle vor. Zeile: " + (i + 2) + "\n"
            + "        Es wird mit dem Absolutwert der Gefälles weiter gerechnet." );
        return true;
      }
    }

    buffer.append( m_meter + ": Profildaten sind O.K.. \n" );
    return true;

  }

  public String getComment( )
  {
    return m_comment;
  }

}