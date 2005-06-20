/**
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestraﬂe 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.lhwsachsenanhalt.tubig;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.logging.Logger;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public final class TubigCalculationData
{
  private final static Logger LOGGER = Logger.getLogger( TubigCalculationData.class.getName() );

  private Date m_startsim;

  // startforecast = aktuelle Modellzeit: beim Zeitreihen holen werden die Daten
  // <= startforecast geholt
  private Date m_startforecast;

  private final Collection m_batches = new ArrayList();

  /**
   * 
   * Liest .calculation (TubigControl) und schreibt die Infos in TubigCalculationData
   * 
   * @throws TubigException
   * 
   * @author Th¸l
   */
  public TubigCalculationData( final URL urlGml ) throws TubigException
  {
    final Feature featGes;
    final Feature featCtrl;
    final FeatureList featLstCtrlBatch;
    Feature featCtrlBatch;
    final Date dtStartSim;
    final Date dtStartForecast;

    Iterator iter;
    try
    {
      // .calculation und zugehˆrige XSD holen (enth‰lt
      // TubigControl/.calculation)
      LOGGER.info( "GML-URL:" + urlGml.toString() );

      featGes = GmlSerializer.createGMLWorkspace( urlGml ).getRootFeature();
      featCtrl = (Feature)featGes.getProperty( "ControlAssociation" );
      //      featMeta = (Feature)featGes.getProperty( "MetadataAssociation" );

      dtStartSim = (Date)featCtrl.getProperty( "startsimulation" );
      dtStartForecast = (Date)featCtrl.getProperty( "startforecast" );
      Feature featCtrlBatchColl;
      featCtrlBatchColl = (Feature)featCtrl.getProperty( "BatchIDCollectionAssociation" );
      featLstCtrlBatch = (FeatureList)featCtrlBatchColl.getProperty( "BatchIDAssociation" );

      // Informationen in TubigCalculationData schreiben
      m_startsim = dtStartSim;
      m_startforecast = dtStartForecast;

      for( iter = featLstCtrlBatch.iterator(); iter.hasNext(); )
      {
        featCtrlBatch = (Feature)iter.next();

        final Object batchid = featCtrlBatch.getProperty( "id" );
        if( batchid != null )
          this.addBatch( batchid.toString() );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new TubigException( "Fehler beim Lesen der Steuerparameter", e );
    }
  }

  public final void addBatch( final String name )
  {
    m_batches.add( name );
  }

  public final String[] getBatches()
  {
    return (String[])m_batches.toArray( new String[m_batches.size()] );
  }

  public final Date getStartforecast()
  {
    return m_startforecast;
  }

  public final Date getStartsim()
  {
    return m_startsim;
  }
}