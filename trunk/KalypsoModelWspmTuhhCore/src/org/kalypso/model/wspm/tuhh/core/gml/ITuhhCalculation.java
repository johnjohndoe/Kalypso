/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.gml;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public interface ITuhhCalculation extends IWspmConstants, IWspmTuhhConstants, Feature
{
  QName QN_PROPERTY_SUB_REACH_DEFINITION_MEMBER = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" ); //$NON-NLS-1$

  QName QN_PROPERTY_FLIESSGESETZ = new QName( NS_WSPM_TUHH, "fliessgesetz" ); //$NON-NLS-1$

  QName QN_PROPERTY_PREFERE_ROUGHNESS_CLASSES = new QName( NS_WSPM_TUHH, "preferRoughnessClasses" ); //$NON-NLS-1$

  QName QN_PROPERTY_STEADY_STATE_MEMBER = new QName( NS_WSPM_TUHH, "reachWspmTuhhSteadyStateMember" ); //$NON-NLS-1$

  QName QN_PROPERTY_RUN_OFF_EVENT_MEMBER = new QName( NS_WSPM_TUHH, "runOffEventMember" ); //$NON-NLS-1$

  QName QN_PROPERTY_WATER_LEVEL_FIXATION_MEMBER = new QName( NS_WSPM_TUHH, "waterLevelFixationMember" ); //$NON-NLS-1$

  QName QN_TUHH_CALC = new QName( NS_WSPM_TUHH, "CalculationWspmTuhhSteadyState" ); //$NON-NLS-1$

  QName QN_TUHH_CALC_REIB_CONST = new QName( NS_WSPM_TUHH, "CalculationReibConstWspmTuhhSteadyState" ); //$NON-NLS-1$

  QName QN_PROP_POLYNOME_MEMBER = new QName( NS_WSPM_TUHH, "calcPolynomesMember" ); //$NON-NLS-1$

  QName QN_PROP_WATERLEVEL_PARAMS = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" ); //$NON-NLS-1$

  QName QN_PROP_EXE_VERSION = new QName( NS_WSPM_TUHH, "exeVersion" ); //$NON-NLS-1$

  QName QN_PROP_SPECIAL_OPTIONS_MEMBER = new QName( NS_WSPM_TUHH, "specialOptionsMember" ); //$NON-NLS-1$

  QName QN_PROP_SPECIAL_PROP_USE_EXTREME_ROUGHNESS = new QName( NS_WSPM_TUHH, "useExtremeRoughness" ); //$NON-NLS-1$

  QName PROPERTY_MODE = new QName( NS_WSPM_TUHH, "mode" ); //$NON-NLS-1$

  public static enum MODE
  {
    WATERLEVEL,
    BF_UNIFORM,
    BF_NON_UNIFORM,
    REIB_KONST;
  }

  public static enum FLIESSGESETZ
  {
    DARCY_WEISBACH_OHNE_FORMEINFLUSS,
    DARCY_WEISBACH_MIT_FORMEINFLUSS,
    MANNING_STRICKLER;
  }

  public static enum START_KONDITION_KIND
  {
    CRITICAL_WATER_DEPTH,
    UNIFORM_BOTTOM_SLOPE,
    WATERLEVEL;
  }

  public static enum WSP_ITERATION_TYPE
  {
    SIMPLE,
    EXACT;
  }

  public static enum VERZOEGERUNSVERLUST_TYPE
  {
    DVWK,
    BJOERNSEN,
    DFG,
    NON;
  }

  public static enum REIBUNGSVERLUST_TYPE
  {
    TRAPEZ_FORMULA,
    GEOMETRIC_FORMULA
  }

  boolean isPreferingRoughnessClasses( );
}
