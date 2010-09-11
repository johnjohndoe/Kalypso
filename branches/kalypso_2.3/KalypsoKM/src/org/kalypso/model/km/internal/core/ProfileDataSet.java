package org.kalypso.model.km.internal.core;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.model.km.internal.i18n.Messages;

public class ProfileDataSet
{
  private final SortedSet<ProfileData> m_profileSort = new TreeSet<ProfileData>( new Comparator<ProfileData>()
      {
    @Override
    public int compare( final ProfileData p1, final ProfileData p2 )
    {
      return Double.compare( p1.getPosition(), p2.getPosition() );
    }
      } );

  private final double m_startPosition;

  private final double m_endPosition;

  private int m_length;

  public ProfileDataSet( final File[] profileFiles )
  {
    init( profileFiles, false );
    m_startPosition = m_profileSort.first().getPosition();
    m_endPosition = m_profileSort.last().getPosition();
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
      final ProfileData qwProfile;
      try
      {
        qwProfile = ProfileFactory.createQWProfile( file, m_startPosition, m_endPosition );
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
      if( i > 0 )
        data.setPrev( profiles[i - 1] );
      if( i + 1 < profiles.length )
        data.setNext( profiles[i + 1] );
    }
  }

  public ProfileData[] getAllProfiles( )
  {
    return m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
  }

  public IKMValue[] getKMValues( ) throws SameXValuesException
  {
    final ProfileData data = m_profileSort.first();

    // generate kmValues for each q;
    final int maxValues = data.getNumberKMValues();
    final List<IKMValue> kmMergedForIndexOverProfiles = new ArrayList<IKMValue>();
    for( int index = 0; index < maxValues; index++ )
    {
      final ProfileData[] profiles = m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
      // collect KMValues for one index (row) over all profiles
      final List<IKMValue> kmForIndexOverProfiles = new ArrayList<IKMValue>();
      for( final ProfileData profile : profiles )
      {
        kmForIndexOverProfiles.add( profile.getKMValue( index ) );
      }
      // merge collection to one kmvalue
      final IKMValue[] kmForIndexOverProfileArray = kmForIndexOverProfiles.toArray( new IKMValue[kmForIndexOverProfiles.size()] );
      kmMergedForIndexOverProfiles.add( new MulitKMValue( kmForIndexOverProfileArray ) );
    }

    final IKMValue[] kmMerged = kmMergedForIndexOverProfiles.toArray( new IKMValue[kmMergedForIndexOverProfiles.size()] );

    final SortedSet<IKMValue> sort = new TreeSet<IKMValue>( new Comparator<IKMValue>()
        {

      @Override
      public int compare( final IKMValue km1, final IKMValue km2 )
      {
        return Double.compare( km1.getQSum(), km2.getQSum() );
      }

        } );
    for( final IKMValue element : kmMerged )
      sort.add( element );

    // Calculate for the number of discharges (at the moment always 5)
    final IKMValue kmFirst = kmMerged[0];
    final IKMValue kmLast = kmMerged[kmMerged.length - 1];
    final double qFirst = kmFirst.getQSum();
    final double qLast = kmLast.getQSum();
    final List<IKMValue> result = new ArrayList<IKMValue>();

    // Use the 5 discharges as follows: first,(first+middle)/2,middle,middle+last)/2,last
    result.add( getKM( sort, qFirst ) );
    m_length = kmMerged.length - 1;
    final int interval = m_length / 4;
    for( int i = 1; i < 4; i++ )
    {
      final double q = kmMerged[i * interval].getQSum();
      result.add( getKM( sort, q ) );
    }
    result.add( getKM( sort, qLast ) );

    return result.toArray( new IKMValue[result.size()] );
  }

  private IKMValue getKM( final SortedSet<IKMValue> sort, final double q ) throws SameXValuesException
  {
    final DummyKMValue value = new DummyKMValue( q );
    final SortedSet<IKMValue> headSet = sort.headSet( value );
    if( headSet.isEmpty() )
    {
      return sort.first();
    }
    final IKMValue km1 = headSet.last();
    final IKMValue km2 = sort.tailSet( value ).first();
    final KMValueFromQinterpolation strandKMValue = new KMValueFromQinterpolation( q, km1, km2 );
    // TODO: check if this is right: Setting n to maximum 30 (Prof.Pasche)
    if( strandKMValue.getN() > 30d )
    {
      final double prod = strandKMValue.getN() * strandKMValue.getK();
      strandKMValue.setN( 30d );
      strandKMValue.setK( prod / 30d );
    }
    if( strandKMValue.getNForeland() > 30d )
    {
      final double prod = strandKMValue.getNForeland() * strandKMValue.getKForeland();
      strandKMValue.setNf( 30d );
      strandKMValue.setKf( prod / 30d );
    }
    return strandKMValue;
  }
}
