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
import org.kalypso.wspwin.core.CalculationContentBeanPasche;
import org.kalypso.wspwin.core.CalculationContentBeanPasche.ART_ANFANGS_WSP;
import org.kalypso.wspwin.core.CalculationContentBeanPasche.FLIESSGESETZ;
import org.kalypso.wspwin.core.CalculationContentBeanPasche.KIND;
import org.kalypso.wspwin.core.ICalculationContentBean;

/**
 * Converts calculations of WspWin Pasche projects to wspm.
 * 
 * @author Gernot Belger
 */
public class CalculationPasche2WspmConverter implements ICalculationWspmConverter
{
  private final String m_baseName;

  private final TuhhReach m_reach;

  private final Map<Integer, String> m_runOffEvents;

  public CalculationPasche2WspmConverter( final TuhhReach reach, final String baseName, final Map<Integer, String> runOffEvents )
  {
    m_reach = reach;
    m_baseName = baseName;
    m_runOffEvents = runOffEvents;
  }

  @Override
  public void convert( final ICalculationContentBean calculation, final File profDir ) throws GMLSchemaException
  {
    final CalculationContentBeanPasche contentBean = (CalculationContentBeanPasche)calculation;
    final CalculationBean bean = contentBean.getCalculationBean();

    // create calculation
    final TuhhWspmProject project = (TuhhWspmProject)m_reach.getWaterBody().getProject();
    final TuhhCalculation calc = project.createCalculation();

    calc.setName( m_baseName + bean.getName() );
    calc.setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.26" ) ); //$NON-NLS-1$
    calc.setCalcCreation( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.27" ), new Date() ); //$NON-NLS-1$
    calc.setReachRef( m_reach );

    final FLIESSGESETZ fliessgesetz = contentBean.getFliessgesetz();
    final org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.FLIESSGESETZ lawOfFlow = findLawOfFlow( fliessgesetz );
    calc.setFliessgesetz( lawOfFlow );

    calc.setSubReachDef( contentBean.getAnfang().doubleValue(), contentBean.getEnde().doubleValue() );

    final ART_ANFANGS_WSP artAnfangswasserspiegel = contentBean.getArtAnfangswasserspiegel();
    final START_KONDITION_KIND type = findStartConditionType( artAnfangswasserspiegel );

    final BigDecimal gefaelle = contentBean.getGefaelle();
    final BigDecimal hoehe = contentBean.getHoehe();

    final Double startWsp = hoehe == null ? null : hoehe.doubleValue();

    calc.setStartCondition( type, startWsp, gefaelle );

    final TuhhCalculation.WSP_ITERATION_TYPE iterationType = findIterationType( contentBean );

    final TuhhCalculation.VERZOEGERUNSVERLUST_TYPE verzType = findVerzoegerungsType( contentBean );

    final TuhhCalculation.REIBUNGSVERLUST_TYPE reibType = findReibType( contentBean );

    final boolean useExtremeRougness = false; // as WSPWIN did not know this option, set to false

    calc.setWaterlevelParameters( iterationType, verzType, reibType, contentBean.isBerechneBruecken(), contentBean.isBerechneWehre(), useExtremeRougness );

    final KIND calcKind = contentBean.getCalcKind();
    final MODE calcMode = findCalcMode( calcKind );
    calc.setCalcMode( calcMode );

    final String runOffRef = m_runOffEvents.get( contentBean.getAbfluss() );
    calc.setRunOffRef( runOffRef );

    // set q-Range. Remember: Q-Range in CalculationcontentBean is in dl/s

    final BigDecimal qMin = contentBean.getMin();
    final BigDecimal qMax = contentBean.getMax();
    final BigDecimal qStep = contentBean.getStep();

    final Double qMinDbl = qMin == null ? null : qMin.doubleValue() / 100.0;
    final Double qMaxDbl = qMax == null ? null : qMax.doubleValue() / 100.0;
    final Double qStepDbl = qStep == null ? null : qStep.doubleValue() / 100.0;

    calc.setQRange( qMinDbl, qMaxDbl, qStepDbl );
  }

  private MODE findCalcMode( final KIND calcKind )
  {
    switch( calcKind )
    {
      case BF_UNIFORM:
        return TuhhCalculation.MODE.BF_UNIFORM;

      case BF_NON_UNIFORM:
        return TuhhCalculation.MODE.BF_NON_UNIFORM;

      case WATERLEVEL:
      default:
        return TuhhCalculation.MODE.WATERLEVEL;
    }
  }

  private WSP_ITERATION_TYPE findIterationType( final CalculationContentBeanPasche contentBean )
  {
    if( contentBean.isSimpleBerechnungWSPInt() )
      return TuhhCalculation.WSP_ITERATION_TYPE.SIMPLE;
    else
      return TuhhCalculation.WSP_ITERATION_TYPE.EXACT;
  }

  private REIBUNGSVERLUST_TYPE findReibType( final CalculationContentBeanPasche contentBean )
  {
    if( contentBean.isReibungsverlustNachTrapezformel() )
      return TuhhCalculation.REIBUNGSVERLUST_TYPE.TRAPEZ_FORMULA;
    else
      return TuhhCalculation.REIBUNGSVERLUST_TYPE.GEOMETRIC_FORMULA;
  }

  private org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.FLIESSGESETZ findLawOfFlow( final FLIESSGESETZ fliessgesetz )
  {
    switch( fliessgesetz )
    {
      case MANNING_STRICKLER:
        return TuhhCalculation.FLIESSGESETZ.MANNING_STRICKLER;

      case DARCY_WEISBACH_OHNE_FORMEINFLUSS:
        return TuhhCalculation.FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS;

      case DARCY_WEISBACH_MIT_FORMEINFLUSS:
      default:
        return TuhhCalculation.FLIESSGESETZ.DARCY_WEISBACH_MIT_FORMEINFLUSS;
    }
  }

  private START_KONDITION_KIND findStartConditionType( final ART_ANFANGS_WSP artAnfangswasserspiegel )
  {
    switch( artAnfangswasserspiegel )
    {
      case DIREKTEINGABE:
        return TuhhCalculation.START_KONDITION_KIND.WATERLEVEL;

      case STATIONAER_GLEICHFOERMIGES_GEFAELLE:
        return TuhhCalculation.START_KONDITION_KIND.UNIFORM_BOTTOM_SLOPE;

      case GRENZTIEFE:
      default:
        return TuhhCalculation.START_KONDITION_KIND.CRITICAL_WATER_DEPTH;
    }
  }

  private VERZOEGERUNSVERLUST_TYPE findVerzoegerungsType( final CalculationContentBeanPasche contentBean )
  {
    switch( contentBean.getVerzoegerungsVerlust() )
    {
      case DFG:
        return TuhhCalculation.VERZOEGERUNSVERLUST_TYPE.DFG;

      case DVWK:
        return TuhhCalculation.VERZOEGERUNSVERLUST_TYPE.DVWK;

      case BJOERNSEN:
      default:
        return TuhhCalculation.VERZOEGERUNSVERLUST_TYPE.BJOERNSEN;
    }
  }
}