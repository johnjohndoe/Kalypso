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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.math.BigDecimal;
import java.util.Date;
import java.util.Map;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.FLIESSGESETZ;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.REIBUNGSVERLUST_TYPE;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.START_KONDITION_KIND;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.VERZOEGERUNSVERLUST_TYPE;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.WSP_ITERATION_TYPE;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.wspwin.core.CalculationBean;
import org.kalypso.wspwin.core.CalculationContentBeanKnauf;
import org.kalypso.wspwin.core.CalculationContentBeanKnauf.LAW_OF_FLOW;
import org.kalypso.wspwin.core.CalculationContentBeanKnauf.START_CONDITION;
import org.kalypso.wspwin.core.ICalculationContentBean;

/**
 * Converts calculations of WspWin Knauf projects to WSPM.
 * 
 * @author Gernot Belger
 */
public class CalculationKnauf2WspmConverter implements ICalculationWspmConverter
{
  private final TuhhReach m_reach;

  private final String m_baseName;

  private final Map<String, String> m_runOffEvents;

  public CalculationKnauf2WspmConverter( final TuhhReach reach, final String baseName, final Map<String, String> runOffEventsByName )
  {
    m_reach = reach;
    m_baseName = baseName;
    m_runOffEvents = runOffEventsByName;
  }

  @Override
  public void convert( final ICalculationContentBean calculation, final File profDir ) throws GMLSchemaException
  {
    final CalculationContentBeanKnauf contentBean = (CalculationContentBeanKnauf) calculation;
    final CalculationBean bean = contentBean.getCalculationBean();

    // create calculation
    final TuhhWspmProject project = (TuhhWspmProject) m_reach.getWaterBody().getProject();
    final TuhhCalculation calc = project.createCalculation();

    calc.setName( m_baseName + bean.getName() );
    calc.setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.26" ) ); //$NON-NLS-1$
    calc.setCalcCreation( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.27" ), new Date() ); //$NON-NLS-1$
    calc.setReachRef( m_reach );

    final FLIESSGESETZ lawOfFlow = findLawOfFlow( contentBean );
    calc.setFliessgesetz( lawOfFlow );

    calc.setSubReachDef( contentBean.getStartStation().doubleValue(), contentBean.getEndStation().doubleValue() );

    final START_KONDITION_KIND type = findStartConditionType( contentBean );

    final double startSlope = contentBean.getStationarySlope().doubleValue();
    final double startWsp = contentBean.getStartWaterlevel().doubleValue();
    calc.setStartCondition( type, startWsp, startSlope );

    final WSP_ITERATION_TYPE iterationType = WSP_ITERATION_TYPE.EXACT;

    final VERZOEGERUNSVERLUST_TYPE verzType = VERZOEGERUNSVERLUST_TYPE.BJOERNSEN;

    final REIBUNGSVERLUST_TYPE reibType = REIBUNGSVERLUST_TYPE.GEOMETRIC_FORMULA;

    final boolean useExtremeRougness = false; // as WSPWIN did not know this option, set to false
    final boolean doCalcBridges = true;
    final boolean doCalcWeirs = true;
    calc.setWaterlevelParameters( iterationType, verzType, reibType, doCalcBridges, doCalcWeirs, useExtremeRougness );

    final MODE calcMode = MODE.WATERLEVEL;
    calc.setCalcMode( calcMode );

    final String runoffName = m_baseName + contentBean.getRunoffName();
    final String runOffRef = m_runOffEvents.get( runoffName );
    calc.setRunOffRef( runOffRef );

    // set q-Range
    final BigDecimal qMin = contentBean.getQMin();
    final BigDecimal qMax = contentBean.getQMax();
    final BigDecimal qStep = contentBean.getQStep();
    if( qMin != null && qMax != null && qStep != null )
      calc.setQRange( qMin.doubleValue(), qMax.doubleValue(), qStep.doubleValue() );
  }

  private START_KONDITION_KIND findStartConditionType( final CalculationContentBeanKnauf contentBean )
  {
    final START_CONDITION startCondition = contentBean.getStartCondition();
    switch( startCondition )
    {
      case HGRENZ:
        return START_KONDITION_KIND.CRITICAL_WATER_DEPTH;

      case HNORM:
        return START_KONDITION_KIND.UNIFORM_BOTTOM_SLOPE;

      case WSP:
      default:
        return START_KONDITION_KIND.WATERLEVEL;
    }
  }

  private FLIESSGESETZ findLawOfFlow( final CalculationContentBeanKnauf contentBean )
  {
    final LAW_OF_FLOW lawOfFlow = contentBean.getLawOfFlow();
    switch( lawOfFlow )
    {
      case MANNING_STRICKLER:
      case EINSTEIN:
        return FLIESSGESETZ.MANNING_STRICKLER;

      case unknown:
      case PRANDTL_COLEBROOK:
      case PC_BEWUCHS_KAISER:
      case PC_BEWUCHS_NUDING:
      case PC_BEWUCHS_MERTENS:
      case PC_BEWUCHS_PASCHE:
      default:
        return FLIESSGESETZ.DARCY_WEISBACH_MIT_FORMEINFLUSS;
    }
  }
}