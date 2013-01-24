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

import org.kalypso.model.hydrology.binding.OverlayElement;
import org.kalypso.model.hydrology.binding.model.Bodenschichtkorrektur;
import org.kalypso.model.hydrology.binding.model.Catchment;
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

  private final Catchment m_sourceCatchment;

  private final CatchmentDissolver m_dissolver;

  private final CatchmentInfos m_infos;

  private final NaModell m_model;

  /**
   * @param infos
   *          Receives the freshly create catchment infos
   */
  public CatchmentResolverWorker( final NaModell model, final Catchment sourceSatchment, final CatchmentDissolver dissolver, final CatchmentInfos infos )
  {
    m_model = model;
    m_sourceCatchment = sourceSatchment;
    m_dissolver = dissolver;
    m_infos = infos;
  }

  public void execute( ) throws NAPreprocessorException
  {
    final DissolvedCatchment[] dissolvedInfos = m_dissolver.getDissolvedInfos( m_sourceCatchment );
    for( final DissolvedCatchment dissolvedInfo : dissolvedInfos )
    {
      // REMARK: it is possible that the whole catchment is covered by the same overlay, so if we only have one, we treat it as the default element
      final boolean singletonCatchment = dissolvedInfos.length == 1;
      final CatchmentInfo info = createDerivedCatchment( dissolvedInfo, singletonCatchment );
      m_infos.addInfo( info );

      m_newCatchments.add( info.getCatchment() );
    }

    /*  */
    // FIXME: split up groundwater inflow relations

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

    /* fix soil type factors: they are not more valid (and will not be used in calc core in any case). To avoid confusion, we reset them to 1.0 */
    resetSoilCorrectionFactors( newCatchment, dissolvedInfo.getOverlay() );

    // TODO: what to change in new catchment?
    // TODO: change links of catchment

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
}