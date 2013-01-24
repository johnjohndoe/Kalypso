/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.hydrology.internal.preprocessing.resolve;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.math.util.MathUtils;
import org.kalypso.model.hydrology.binding.OverlayElement;
import org.kalypso.model.hydrology.binding.model.Bodenschichtkorrektur;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Grundwasserabfluss;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.parameter.DRWBMDefinition;
import org.kalypso.model.hydrology.binding.parameter.DRWBMSoilLayerParameter;
import org.kalypso.model.hydrology.binding.parameter.DRWBMSoiltype;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;

/**
 * Does the real work in splitting up one catchment into a main catchment and several sub-catchments according to drwbm.
 * 
 * @author Gernot Belger
 */
class CatchmentResolverWorker
{
  /* rememeber the new catchments we need them later one */
  private final Set<Catchment> m_newCatchments = new HashSet<>();

  private final Catchment m_mainCatchment;

  private final CatchmentDissolver m_dissolver;

  private final CatchmentInfos m_infos;

  private final NaModell m_model;

  private final CatchmentGroundwaterFinder m_groundwaterFinder;

  /**
   * @param infos
   *          Receives the freshly create catchment infos
   */
  public CatchmentResolverWorker( final NaModell model, final Catchment sourceSatchment, final CatchmentDissolver dissolver, final CatchmentGroundwaterFinder groundwaterFinder, final CatchmentInfos infos )
  {
    m_model = model;
    m_mainCatchment = sourceSatchment;
    m_dissolver = dissolver;
    m_groundwaterFinder = groundwaterFinder;
    m_infos = infos;
  }

  public void execute( ) throws NAPreprocessorException
  {

    /* create the new catchments, if any */
    final double totalArea = createDerivedCatchments();

    /* fix groundwater inflow relations for splitted catchments */
    if( m_newCatchments.size() > 0 )
      fixGroundwaterRelations( totalArea );
  }

  private double createDerivedCatchments( ) throws NAPreprocessorException
  {
    /* total area of all involved catchments/hydrotopes (=area of the original catchment) */
    double totalArea = 0.0;
    final DissolvedCatchment[] dissolvedInfos = m_dissolver.getDissolvedInfos( m_mainCatchment );
    for( final DissolvedCatchment dissolvedInfo : dissolvedInfos )
    {
      totalArea += dissolvedInfo.getHydrotopeArea();

      // REMARK: it is possible that the whole catchment is covered by the same overlay, so if we only have one, we treat it as the default element
      final boolean singletonCatchment = dissolvedInfos.length == 1;
      final CatchmentInfo info = createDerivedCatchment( dissolvedInfo, singletonCatchment );
      m_infos.addInfo( info );

      m_newCatchments.add( info.getCatchment() );
    }
    m_newCatchments.remove( m_mainCatchment );

    return totalArea;
  }

  private CatchmentInfo createDerivedCatchment( final DissolvedCatchment dissolvedInfo, final boolean singletonCatchment ) throws NAPreprocessorException
  {
    if( !dissolvedInfo.hasDrwbm() || singletonCatchment )
    {
      // nothing to do, old catchment remains
      return dissolvedInfo.createInfo();
    }

    final Catchment catchment = dissolvedInfo.getCatchment();

    final NaModelManipulator manipulator = new NaModelManipulator( m_model );
    final Catchment newCatchment = manipulator.insertClonedCatchment( catchment );

    /* changes some properties */
    newCatchment.setName( dissolvedInfo.getLabel() );

    /* changes do to overwritten parameters in overlay */
    // - soil parameters: no need to do anything: the changes soil parameters are part of the hydrotopes
    // - TODO: change links of catchment, if overlay overwrites them;

    /* fix soil type factors: they are not more valid (and will not be used in calc core in any case). To avoid confusion, we reset them to 1.0 */
    resetSoilCorrectionFactors( newCatchment, dissolvedInfo.getOverlay() );

    return dissolvedInfo.createInfo( newCatchment );
  }

  private void resetSoilCorrectionFactors( final Catchment newCatchment, final OverlayElement overlay )
  {
    final IFeatureBindingCollection<Bodenschichtkorrektur> bodenKorrekturCollection = newCatchment.getBodenKorrekturCollection();
    bodenKorrekturCollection.clear();

    // REAMRK: no null checks needed, if we get here, we have a drainage overlay
    final IXLinkedFeature drwbmLink = overlay.getDRWBMDefinition();
    final DRWBMDefinition drwbmDefinition = (DRWBMDefinition)drwbmLink.getFeature();
    final IXLinkedFeature soiltypeLink = drwbmDefinition.getSoiltype();
    final DRWBMSoiltype soilType = (DRWBMSoiltype)soiltypeLink.getFeature();
    final IFeatureBindingCollection<DRWBMSoilLayerParameter> parameters = soilType.getParameters();

    /* create as many factors as we have parameters */
    final int size = parameters.size();
    for( int i = 0; i < size; i++ )
      bodenKorrekturCollection.addNew( Bodenschichtkorrektur.FEATURE_BODENSCHICHTKORREKTUR );
  }

  /**
   * If the main catchment has incoming groundwater relations, each such relation is split up into several relations targeting the main catchment and its new sub-catchments.<br/>
   * Their factors are readjusted according to the relative weight of each such catchment in relation to the former toal area of the main catchment.
   */
  private void fixGroundwaterRelations( final double totalArea )
  {
    /* reduced area of source catchment */
    final CatchmentInfo mainInfo = m_infos.getInfo( m_mainCatchment );
    final double mainArea = mainInfo.getTotalSealing().getArea();

    final Pair<Catchment, Grundwasserabfluss>[] relatingCatchments = m_groundwaterFinder.findIncomingGroundwaterRelations( m_mainCatchment );
    for( final Pair<Catchment, Grundwasserabfluss> relatingCatchment : relatingCatchments )
    {
      final Catchment source = relatingCatchment.getKey();
      final Grundwasserabfluss originalGroundwater = relatingCatchment.getValue();
      final double originalFactor = originalGroundwater.getGwwi();

      /* reduce factor of main catchment according to reduced area */
      final double mainAreaWeight = mainArea / totalArea;
      final double mainFactor = originalFactor * mainAreaWeight;
      originalGroundwater.setGwwi( MathUtils.round( mainFactor, 3 ) );

      /* create new relation for each new catchment */
      final IFeatureBindingCollection<Grundwasserabfluss> groundwaterCollection = source.getGrundwasserAbflussCollection();
      for( final Catchment newCatchment : m_newCatchments )
      {
        final Grundwasserabfluss newGroundwaterElement = groundwaterCollection.addNew( Grundwasserabfluss.FEATURE_GRUNDWASSERABFLUSS );
        newGroundwaterElement.setNgwzu( newCatchment );

        final CatchmentInfo newInfo = m_infos.getInfo( newCatchment );
        final double newArea = newInfo.getTotalSealing().getArea();
        final double newAreaWeight = newArea / totalArea;
        final double newFactor = originalFactor * newAreaWeight;
        newGroundwaterElement.setGwwi( MathUtils.round( newFactor, 3 ) );
      }
    }
  }
}