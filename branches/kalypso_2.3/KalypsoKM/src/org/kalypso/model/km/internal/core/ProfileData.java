package org.kalypso.model.km.internal.core;

import java.io.File;

import org.kalypso.model.km.internal.i18n.Messages;

public class ProfileData
{
  private final double m_meter;

  private ProfileData m_nextProfile;

  private ProfileData m_prevProfile;

  private final double m_min;

  private final double m_max;

  private Row[] m_rows;

  private final File m_file;

  public ProfileData( final File file, final double min, final double max, final double meter )
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

  public double getRange( final double minPos, final double maxPos )
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
    return resultMax - resultMin;
  }

  @Override
  public String toString( )
  {
    final StringBuffer result = new StringBuffer( Messages.getString( "org.kalypso.model.km.ProfileData.0" ) + m_meter + Messages.getString( "org.kalypso.model.km.ProfileData.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    for( final Row row : m_rows )
    {
      result.append( row.toString() );
      result.append( "\n" ); //$NON-NLS-1$

    }
    return result.toString();
  }

  public double getPosition( )
  {
    return m_meter;
  }

  public void setPrev( final ProfileData profile )
  {
    m_prevProfile = profile;
  }

  public void setNext( final ProfileData profile )
  {
    m_nextProfile = profile;
  }

  public void set( final Row[] rowArray )
  {
    m_rows = rowArray;
  }

  public int getNumberKMValues( )
  {
    return m_rows.length - 1;
  }

  public IKMValue getKMValue( final int index )
  {
    return new KMValue( getRange( m_min, m_max ), m_rows[index], m_rows[index + 1] );
  }

  public String isValidForKalypso( )
  {
    if( m_rows.length < 6 ) // not enough values for calculation
      return m_meter + Messages.getString( "org.kalypso.model.km.ProfileData.3" ); //$NON-NLS-1$

    for( int i = 0; i < m_rows.length - 1; i++ )
    {
      if( (m_rows[i].getQ() - m_rows[i + 1].getQ()) > 0.009 )
      {
        // Änderung in der dritten Nachkommastelle O.K. -
        // Rechenungenauigkeiten im Hydraulikmodell
        return Messages.getString( "org.kalypso.model.km.ProfileData.4" ) + (i + 2); //$NON-NLS-1$
      }
    }

    // TODO: Check if values above bordfull is necessary!
    if( m_rows[m_rows.length - 1].getAlpha() >= 1.0 ) // no values above bordfull
      return Messages.getString( "org.kalypso.model.km.ProfileData.6" ); //$NON-NLS-1$

    for( int i = 0; i < m_rows.length - 1; i++ )
    {
      if( m_rows[i].getSlope() < 0.0 )
      {
        return Messages.getString( "org.kalypso.model.km.ProfileData.7" ) + (i + 2) //$NON-NLS-1$ 
        + Messages.getString( "org.kalypso.model.km.ProfileData.9" ); //$NON-NLS-1$
      }
    }

    return null;
  }

}