package org.kalypso.model.km;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.model.km.i18n.Messages;

public class ProfileDataSet
{
  private final SortedSet<ProfileData> m_profileSort = new TreeSet<ProfileData>( new Comparator<ProfileData>()
  {
    public int compare( ProfileData p1, ProfileData p2 )
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

  public ProfileDataSet( final File[] profileFiles, double min, double max )
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

  private void init( final File[] profileFiles, boolean validatePosition )
  {
    for( int i = 0; i < profileFiles.length; i++ )
    {
      final File file = profileFiles[i];
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
      catch( IOException e )
      {
        Logger.getAnonymousLogger().log( Level.WARNING, Messages.getString("org.kalypso.model.km.ProfileDataSet.0") + file.getAbsolutePath() ); //$NON-NLS-1$
        e.printStackTrace();
      }
    }
    final ProfileData[] profiles = m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
    for( int i = 0; i < profiles.length; i++ )
    {
      ProfileData data = profiles[i];
      if( i > 0 )
        data.setPrev( profiles[i - 1] );
      if( i + 1 < profiles.length )
        data.setNext( profiles[i + 1] );
    }
  }

  public Iterator<ProfileData> getAllProfiles( )
  {
    return m_profileSort.iterator();
  }

  public AbstractKMValue[] getKMValues( ) throws SameXValuesException
  {
    final ProfileData data = m_profileSort.first();

    // generate kmValues for each q;
    int maxValues = data.getNumberKMValues();
    final List<AbstractKMValue> kmMergedForIndexOverProfiles = new ArrayList<AbstractKMValue>();
    for( int index = 0; index < maxValues; index++ )
    {
      final ProfileData[] profiles = (ProfileData[]) m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
      // collect KMValues for one index (row) over all profiles
      final List<AbstractKMValue> kmForIndexOverProfiles = new ArrayList<AbstractKMValue>();
      for( int counterProfile = 0; counterProfile < profiles.length; counterProfile++ )
      {
        final ProfileData profile = profiles[counterProfile];
        kmForIndexOverProfiles.add( profile.getKMValue( index ) );
      }
      // merge collection to one kmvalue
      final AbstractKMValue[] kmForIndexOverProfileArray = (AbstractKMValue[]) kmForIndexOverProfiles.toArray( new AbstractKMValue[kmForIndexOverProfiles.size()] );
      kmMergedForIndexOverProfiles.add( new MulitKMValue( kmForIndexOverProfileArray ) );
    }

    final AbstractKMValue[] kmMerged = (AbstractKMValue[]) kmMergedForIndexOverProfiles.toArray( new AbstractKMValue[kmMergedForIndexOverProfiles.size()] );

    final SortedSet<AbstractKMValue> sort = new TreeSet<AbstractKMValue>( new Comparator<AbstractKMValue>()
    {

      public int compare( AbstractKMValue km1, AbstractKMValue km2 )
      {
        return Double.compare( km1.getQSum(), km2.getQSum() );
      }

    } );
    for( int i = 0; i < kmMerged.length; i++ )
      sort.add( kmMerged[i] );

    // Calculate for the number of discharges (at the moment always 5)
    final AbstractKMValue kmFirst = kmMerged[0];
    final AbstractKMValue kmLast = kmMerged[kmMerged.length - 1];
    double qFirst = kmFirst.getQSum();
    double qLast = kmLast.getQSum();
    final List<AbstractKMValue> result = new ArrayList<AbstractKMValue>();

    // Use the 5 discharges as follows: first,(first+middle)/2,middle,middle+last)/2,last
    result.add( getKM( sort, qFirst ) );
    m_length = kmMerged.length - 1;
    int interval = m_length / 4;
    for( int i = 1; i < 4; i++ )
    {
      double q = kmMerged[i * interval].getQSum();
      result.add( getKM( sort, q ) );
    }
    result.add( getKM( sort, qLast ) );

    return (AbstractKMValue[]) result.toArray( new AbstractKMValue[result.size()] );
  }

  private AbstractKMValue getKM( SortedSet<AbstractKMValue> sort, double q ) throws SameXValuesException
  {

    final DummyKMValue value = new DummyKMValue( q );
    final SortedSet<AbstractKMValue> headSet = sort.headSet( value );
    if( headSet.isEmpty() )
    {
      return sort.first();
    }
    final AbstractKMValue km1 = headSet.last();
    final AbstractKMValue km2 = sort.tailSet( value ).first();
    KMValueFromQinterpolation strandKMValue = new KMValueFromQinterpolation( q, km1, km2 );
    // TODO: check if this is right: Setting n to maximum 30 (Prof.Pasche)
    if( strandKMValue.getN() > 30d )
    {
      double prod = strandKMValue.getN() * strandKMValue.getK();
      strandKMValue.setN( 30d );
      strandKMValue.setK( prod / 30d );
    }
    if( strandKMValue.getNForeland() > 30d )
    {
      double prod = strandKMValue.getNForeland() * strandKMValue.getKForeland();
      strandKMValue.setNf( 30d );
      strandKMValue.setKf( prod / 30d );
    }
    return strandKMValue;
  }

  private class DummyKMValue extends AbstractKMValue
  {
    private final double m_q;

    public DummyKMValue( double q )
    {
      m_q = q;
    }

    public double getLength( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getAlpha( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getK( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getN( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getKForeland( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getNForeland( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getQ( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getQForeland( )
    {
      // TODO Auto-generated method stub
      return 0;
    }

    public double getQSum( )
    {
      return m_q;
    }

  }
}
