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
package org.kalypso.kalypsomodel1d2d.schema.dict;

/**
 * @author Dejan Antanaskovic
 */
public interface Kalypso1D2DDictConstants
{
  public static final String DICT_COMPONENT_ORDINAL_NUMBER = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_TIME = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_UNDER_RELAXATION_FACTOR = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_WATERLEVEL = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_WAVE_HSIG = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#WaveHsig"; //$NON-NLS-1$
  
  public static final String DICT_COMPONENT_WAVE_DIR = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#WaveDirection"; //$NON-NLS-1$
  
  public static final String DICT_COMPONENT_WAVE_PER = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#WavePeakPeriod"; //$NON-NLS-1$
  
  public static final String DICT_COMPONENT_WAVE_DD = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#WaveDirectionalSpreading"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_WATERLEVEL_UPSTREAM = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#WaterlevelUpstream"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_WATERLEVEL_DOWNSTREAM = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#WaterlevelDownstream"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_DISCHARGE = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_SPECIFIC_DISCHARGE_1D = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#SpecificDischarge1D"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_SPECIFIC_DISCHARGE_2D = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#SpecificDischarge2D"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_DEPTH = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Depth"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_VELOCITY = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Velocity"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_VELOCITY_DIRECTION = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#VelocityDirection"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_MAX_VELOCITY_X = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityX"; //$NON-NLS-1$

  public static final String DICT_COMPONENT_MAX_VELOCITY_Y = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityY"; //$NON-NLS-1$

  public static final Object DICT_COMPONENT_MAX_VELOCITY_NODE = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityNode"; //$NON-NLS-1$

  public static final Object DICT_COMPONENT_MAX_DEPTH = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxDepth"; //$NON-NLS-1$

  public static final Object DICT_COMPONENT_MAX_DEPTH_NODE = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxDepthNode"; //$NON-NLS-1$

  public static final Object DICT_COMPONENT_AVERAGE_VELOCITY_X = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageVelocityX"; //$NON-NLS-1$

  public static final Object DICT_COMPONENT_AVERAGE_VELOCITY_Y = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageVelocityY"; //$NON-NLS-1$

  public static final Object DICT_COMPONENT_AVERAGE_DEPTH = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageDepth"; //$NON-NLS-1$

  public static final Object DICT_COMPONENT_STEP_INFO_SWAN = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#StepInfoText"; //$NON-NLS-1$
}
