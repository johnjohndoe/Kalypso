package org.kalypso.model.km.internal.core;

import java.io.File;

import org.kalypso.model.km.internal.i18n.Messages;

public class ProfileData
{
  private final double m_meter;

  private final double m_min;

  private final double m_max;

  private Row[] m_rows;

  private final File m_file;

  private double m_range;

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

  // FIXME: instead of setting prev/next and later calculate range: directly set range from outside
  public double calculateRange( final ProfileData prevProfile, final ProfileData nextProfile )
  {
    final double resultMax;
    final double resultMin;

    if( nextProfile == null )
      resultMax = m_max;
    else
      resultMax = Math.min( getPosition() + (nextProfile.getPosition() - getPosition()) / 2d, m_max );
    if( prevProfile == null )
      resultMin = m_min;
    else
      resultMin = Math.max( getPosition() - (getPosition() - prevProfile.getPosition()) / 2d, m_min );

    return resultMax - resultMin;
  }

  private double getRange( )
  {
    return m_range;
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
    return new KMValue( getRange(), m_rows[index], m_rows[index + 1] );
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

  public void setRange( final double range )
  {
    m_range = range;
  }

}