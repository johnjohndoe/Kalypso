/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
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
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.wiskiadapter;

import java.util.Date;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.manipulator.IObservationManipulator;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;

/**
 * WiskiObservationManipulator is able to extend the metadata information of an IObservation
 * 
 * @author schlienger (31.05.2005)
 */
public class WiskiObservationManipulator implements IObservationManipulator
{
  /**
   * This implementation tries to fetch the RatingTable from the file-cache and eventually updates the observation
   * metadata.
   * <p>
   * The data argument should be a string denoting the tsInfoName.
   * 
   * @see org.kalypso.ogc.sensor.manipulator.IObservationManipulator#manipulate(org.kalypso.ogc.sensor.IObservation,
   *      java.lang.Object)
   */
  public void manipulate( final IObservation obs, final Object data ) throws SensorException
  {
    if( obs == null || data == null || !( data instanceof String ) )
      return;

    // does nothing if WQ-Stuff already here
    if( obs.getMetadataList().containsKey( TimeserieConstants.MD_WQTABLE ) )
      return;

    // no WQ-Information, try to load it from the file cache
    final String tsInfoName = (String)data;

    final WQTableSet set = RatingTableCache.getInstance().get( tsInfoName, new Date() );
    if( set == null )
      return;

    final String xml;
    try
    {
      xml = WQTableFactory.createXMLString( set );
      obs.getMetadataList().setProperty( TimeserieConstants.MD_WQTABLE, xml );
    }
    catch( final WQException e )
    {
      throw new SensorException( e );
    }
  }
}
