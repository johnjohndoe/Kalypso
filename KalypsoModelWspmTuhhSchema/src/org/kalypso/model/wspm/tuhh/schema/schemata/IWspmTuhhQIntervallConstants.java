/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.schema.schemata;

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * Constants for the use of the QIntervall-Schema
 * 
 * @author Gernot Belger
 */
public interface IWspmTuhhQIntervallConstants extends IWspmTuhhConstants
{
  String DICT_BASE = "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#"; //$NON-NLS-1$

  String DICT_PHENOMENON_WATERLEVEL = DICT_BASE + "phenomenonWaterlevel"; //$NON-NLS-1$

  String DICT_PHENOMENON_RUNOFF = DICT_BASE + "phenomenonRunoff"; //$NON-NLS-1$

  String DICT_PHENOMENON_AREA = DICT_BASE + "phenomenonArea"; //$NON-NLS-1$

  String DICT_PHENOMENON_ALPHA = DICT_BASE + "phenomenonAlpha"; //$NON-NLS-1$

  String DICT_COMPONENT_WATERLEVEL = DICT_BASE + "Waterlevel"; //$NON-NLS-1$

  String DICT_COMPONENT_WATERLEVEL_UPSTREAM = DICT_BASE + "WaterlevelUpstream"; //$NON-NLS-1$

  String DICT_COMPONENT_WATERLEVEL_DOWNSTREAM = DICT_BASE + "WaterlevelDownstream"; //$NON-NLS-1$

  String DICT_COMPONENT_DEPTH = DICT_BASE + "Depth"; //$NON-NLS-1$

  String DICT_COMPONENT_AREA = DICT_BASE + "Area"; //$NON-NLS-1$

  String DICT_COMPONENT_RUNOFF = DICT_BASE + "Runoff"; //$NON-NLS-1$

  String DICT_COMPONENT_ALPHA = DICT_BASE + "Alpha"; //$NON-NLS-1$
  
  String DICT_COMPONENT_WIDTH = DICT_BASE + "Width"; //$NON-NLS-1$

  String DICT_COMPONENT_DELTA_AREA = DICT_BASE + "DeltaArea"; //$NON-NLS-1$

  String DICT_COMPONENT_DELTA_RUNOFF = DICT_BASE + "DeltaRunoff"; //$NON-NLS-1$

  String DICT_COMPONENT_DELTA_ALPHA = DICT_BASE + "DeltaAlpha"; //$NON-NLS-1$

  String DICT_COMPONENT_RUNOFF_CHANNEL = DICT_BASE + "RunoffChannel"; //$NON-NLS-1$

  String DICT_COMPONENT_RUNOFF_FLOODPLAIN = DICT_BASE + "RunoffFloodplain"; //$NON-NLS-1$

  String DICT_COMPONENT_AREA_CHANNEL = DICT_BASE + "AreaChannel"; //$NON-NLS-1$

  String DICT_COMPONENT_AREA_FLOODPLAIN = DICT_BASE + "AreaFloodplain"; //$NON-NLS-1$

  String DICT_COMPONENT_WIDTH_CHANNEL = DICT_BASE + "WidthChannel"; //$NON-NLS-1$

  String DICT_COMPONENT_WIDTH_FLOODPLAIN = DICT_BASE + "WidthFloodplain"; //$NON-NLS-1$
}