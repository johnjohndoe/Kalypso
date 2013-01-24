package org.kalypso.model.km.internal.core;

import java.io.File;
import java.io.IOException;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.km.internal.i18n.Messages;

public class ProfileDataSet
{
  private final SortedSet<ProfileData> m_profileSort = new TreeSet<ProfileData>( new ProfileDataComparator() );

  private final double m_startPosition;

  private final double m_endPosition;

  public ProfileDataSet( final File[] profileFiles )
  {
    init( profileFiles, false );

    if( profileFiles.length > 0 )
    {
      m_startPosition = m_profileSort.first().getPosition();
      m_endPosition = m_profileSort.last().getPosition();
    }
    else
    {
      m_startPosition = Double.NaN;
      m_endPosition = Double.NaN;
    }
  }

  public ProfileDataSet( final File[] profileFiles, final double min, final double max )
  {
    m_startPosition = min;
    m_endPosition = max;
    init( profileFiles, true );
  }

  public double getStartPosition( )
  {
    return m_startPosition;
  }

  public double getEndPosition( )
  {
    return m_endPosition;
  }

  private void init( final File[] profileFiles, final boolean validatePosition )
  {
    for( final File file : profileFiles )
    {
      try
      {
        final ProfileData qwProfile = ProfileFactory.createQWProfile( file, m_startPosition, m_endPosition );
        if( validatePosition )
        {
          final double profilePos = qwProfile.getPosition();
          if( m_startPosition <= profilePos && profilePos <= m_endPosition )
            m_profileSort.add( qwProfile );
        }
        else
          m_profileSort.add( qwProfile );
      }
      catch( final IOException e )
      {
        Logger.getAnonymousLogger().log( Level.WARNING, Messages.getString( "org.kalypso.model.km.ProfileDataSet.0" ) + file.getAbsolutePath() ); //$NON-NLS-1$
        e.printStackTrace();
      }
    }

    final ProfileData[] profiles = m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
    for( int i = 0; i < profiles.length; i++ )
    {
      final ProfileData data = profiles[i];
      final ProfileData prevProfile = i == 0 || profiles.length == 1 ? null : profiles[i - 1];
      final ProfileData nextProfile = i + 1 < profiles.length ? profiles[i + 1] : null;
      final double range = data.calculateRange( prevProfile, nextProfile );
      data.setRange( range );
    }
  }

  public ProfileData[] getAllProfiles( )
  {
    return m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
  }

  public IKMValue[] getKMValues( )
  {
    final ProfileData data = m_profileSort.first();

    // generate kmValues for each q;
    final int numKMValues = data.getNumberKMValues();
    final IKMValue[] kmMerged = new IKMValue[numKMValues];
    for( int index = 0; index < numKMValues; index++ )
    {
      final IKMValue kmValue = getKMValue( index );
      kmMerged[index] = kmValue;
    }

    final SortedSet<IKMValue> sort = new TreeSet<IKMValue>( new KMValueComparator() );
    for( final IKMValue element : kmMerged )
      sort.add( element );

    // FIXME: looks not natural we just pick discharges by chance (fixed interval length); shoudn't we combine the
    // intermediate values?

    // Calculate for the number of discharges (at the moment always 5)
    final IKMValue kmFirst = kmMerged[0];
    final IKMValue kmLast = kmMerged[kmMerged.length - 1];

    // TODO: dubious 2: we interpolate the index ?! Instead we should devide the q-range by 5, and use these q's!

    // Use the 5 discharges as follows: first,(first+middle)/2,middle,middle+last)/2,last
    final int interval = (kmMerged.length - 1) / 4;

    final IKMValue[] result = new IKMValue[5];
    // REMARK: as we just pick existing discharges, interpolation actually makes no sense; maybe, later, if we really
    // pick intermediate discharges, we should interpolate again
    result[0] = kmFirst;
    result[1] = kmMerged[1 * interval];
    result[2] = kmMerged[2 * interval];
    result[3] = kmMerged[3 * interval];
    result[4] = kmLast;
// result[0] = KMValueFromQinterpolation.interpolateKM( sort, kmFirst.getQSum() );
// result[1] = KMValueFromQinterpolation.interpolateKM( sort, kmMerged[1 * interval].getQSum() );
// result[2] = KMValueFromQinterpolation.interpolateKM( sort, kmMerged[2 * interval].getQSum() );
// result[3] = KMValueFromQinterpolation.interpolateKM( sort, kmMerged[3 * interval].getQSum() );
// result[4] = KMValueFromQinterpolation.interpolateKM( sort, kmLast.getQSum() );

    return result;
  }

  // collect KMValues for one index (row) over all profiles
  private IKMValue getKMValue( final int index )
  {
    final ProfileData[] profiles = m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
    final IKMValue[] profileKMs = new IKMValue[profiles.length];

    for( int i = 0; i < profileKMs.length; i++ )
      profileKMs[i] = profiles[i].getKMValue( index );

    return new MultiKMValue( profileKMs );
  }
}
