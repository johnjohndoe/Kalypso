package org.kalypso.psiadapter.repository;

import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.factory.AbstractRepositoryFactory;

import de.psi.go.lhwz.PSICompact.WQData;
import de.psi.go.lhwz.PSICompact.WQParamSet;

/**
 * NTH: the configuration (as in resources/config.ini) should come from "outside", for instance localised using an url,
 * so that this adpater can be configured more dynamically
 * 
 * @author schlienger
 */
public class PSICompactRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * Does nothing.
   * 
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository( )
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository( ) throws RepositoryException
  {
    return new PSICompactRepository( "PSICompact", false );
  }

  /**
   * Helper that converts PSICompact WQParamSet objects to a WechmannSets object.
   * 
   * @return WechmannGroup constructed from the WQParamSet array
   */
  public static WechmannGroup readWQParams( final WQParamSet[] pset )
  {
    final WechmannSet[] wsets = new WechmannSet[pset.length];
    for( int i = 0; i < pset.length; i++ )
    {
      final WQData[] ds = pset[i].getWqData();
      final WechmannParams[] wps = new WechmannParams[ds.length];

      for( int j = 0; j < ds.length; j++ )
        wps[j] = new WechmannParams( ds[j].getW1(), ds[j].getLNK1(), ds[j].getK2(), ds[j].getWGR() );

      wsets[i] = new WechmannSet( pset[i].getValidFrom(), wps );
    }

    return new WechmannGroup( wsets );
  }
}