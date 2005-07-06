/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor;

import org.kalypso.commons.xml.xlink.IXlink;
import org.kalypso.ogc.sensor.request.IRequest;

/**
 * Eine sog. Observation im Sinne von OGC Sensor-ML. Beschreibt eine maschinelle oder menschlische Wert-Erfassung.
 * 
 * @author schlienger
 */
public interface IObservation extends IObservationEventProvider
{
  /**
   * Returns the identifier of this Observation. The identifier can be used to uniquely identify the Observation within
   * its repository.
   * 
   * @return identifier
   */
  public String getIdentifier();

  /**
   * Returns the name of this Observation
   * 
   * @return name
   */
  public String getName();

  /**
   * Returns true if this observation is editable.
   * 
   * @return editable flag
   */
  public boolean isEditable();

  /**
   * Returns the target object for which this observation has measurements.
   * 
   * @return target or null
   */
  public IXlink getTarget();

  /**
   * Returns the list of Metadata.
   * 
   * @return metadata
   */
  public MetadataList getMetadataList();

  /**
   * Returns the list of axis
   * 
   * @return axes array
   */
  public IAxis[] getAxisList();

  /**
   * Returns the values resulting from the measurements this observation stands for.
   * 
   * @param args
   *          some client defined arguments that can be interpretated by the implementation. Implementors of this
   *          interface can use this parameter, but they are not forced to do so.
   * @return model
   * @throws SensorException
   */
  public ITuppleModel getValues( final IRequest args ) throws SensorException;

  /**
   * Sets the given values.
   * 
   * @param values
   * @throws SensorException
   */
  public void setValues( final ITuppleModel values ) throws SensorException;

  /**
   * Returns the localisation of the base file behind this observation
   * 
   * @return href
   */
  public String getHref();
}