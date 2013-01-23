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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.OverlayElement;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentIndex;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * Holds information about the catchments derived from the hydrotopes.
 * 
 * @author Gernot Belger
 */
class CatchmentDissolver
{
  private final Map<Catchment, Collection<DissolvedCatchment>> m_catchmentHash = new HashMap<>();

  private final Map<String, DissolvedCatchment> m_dissolvedCatchments = new HashMap<>();

  private final ParameterHash m_landuseHash;

  private final Catchment[] m_catchments;

  private final CatchmentIndex m_catchmentIndex;

  public CatchmentDissolver( final ParameterHash landuseHash, final NaModell model )
  {
    m_landuseHash = landuseHash;

    final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();
    m_catchments = catchments.toArray( new Catchment[catchments.size()] );
    m_catchmentIndex = new CatchmentIndex( m_catchments );
  }

  public Catchment[] getCatchments( )
  {
    return m_catchments;
  }

  public IStatus addHydrotopes( final HydrotopeCollection hydrotopeCollection ) throws NAPreprocessorException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final IFeatureBindingCollection<IHydrotope> hydrotopes = hydrotopeCollection.getHydrotopes();
    for( final IHydrotope hydrotope : hydrotopes )
    {
      final Catchment originalCatchment = m_catchmentIndex.findCatchment( hydrotope );
      if( originalCatchment == null )
      {
        final String message = String.format( Messages.getString( "NaCatchmentData_0" ) ); //$NON-NLS-1$
        log.add( IStatus.WARNING, message );
      }
      else
        addHydrotope( hydrotope, originalCatchment );
    }

    validateArea( log );

    return log.asMultiStatus( Messages.getString( "NaCatchmentData_1" ) ); //$NON-NLS-1$
  }

  private void addHydrotope( final IHydrotope hydrotope, final Catchment catchment ) throws NAPreprocessorException
  {
    /* create and validate hydrotope info */
    final HydrotopeInfo hydrotopeInfo = new HydrotopeInfo( hydrotope, m_landuseHash );
    hydrotopeInfo.validateAttributes();

    /* build key */
    final OverlayElement overlay = getDrwbmOverly( hydrotopeInfo );
    final String hashKey = buildHashKey( overlay, catchment );
    final DissolvedCatchment dissolved = getOrCreateDissolved( catchment, hashKey, overlay );
    dissolved.add( hydrotopeInfo );
  }

  /* We only separate catchments, if they have a drwbm definition comming from an overlay. */
  private OverlayElement getDrwbmOverly( final HydrotopeInfo hydrotopeInfo )
  {
    final boolean isDrwbm = hydrotopeInfo.isDrwbmOverlay();
    if( isDrwbm )
    {
      final OverlayElement overlay = hydrotopeInfo.getLinkedOverlay();

      // REMARK: only hydrotopes with overlay have a drwbm definition
      Assert.isNotNull( overlay );

      return overlay;
    }

    return null;
  }

  private DissolvedCatchment getOrCreateDissolved( final Catchment catchment, final String hashKey, final OverlayElement overlay )
  {
    final DissolvedCatchment existingDissolver = m_dissolvedCatchments.get( hashKey );
    if( existingDissolver != null )
      return existingDissolver;

    final DissolvedCatchment newDissolver = new DissolvedCatchment( catchment, overlay );
    m_dissolvedCatchments.put( hashKey, newDissolver );

    /* keep track of all dissolver that belong to the same catchment */
    if( !m_catchmentHash.containsKey( catchment ) )
      m_catchmentHash.put( catchment, new ArrayList<DissolvedCatchment>() );

    final Collection<DissolvedCatchment> dissolvedElements = m_catchmentHash.get( catchment );
    dissolvedElements.add( newDissolver );

    return newDissolver;
  }

  private String buildHashKey( final OverlayElement overlay, final Catchment catchment )
  {
    final String catchmentID = catchment.getId();

    /* exactly the overlays having a drainage function will go into an own (new) catchment */
    if( overlay == null )
      return catchmentID;

    if( !overlay.hasDrainageFunction() )
      return catchmentID;

    final String overlayID = overlay.getId();
    return String.format( "%s#%s", catchmentID, overlayID );
  }

  private void validateArea( final IStatusCollector log )
  {
    for( final Entry<Catchment, Collection<DissolvedCatchment>> entry : m_catchmentHash.entrySet() )
    {
      final Catchment catchment = entry.getKey();

      /* calculate total hydrotope area for this catchment */
      double totalHydrotopeArea = 0;
      final Collection<DissolvedCatchment> infos = entry.getValue();
      for( final DissolvedCatchment info : infos )
      {
        final double hydrotopeArea = info.getHydrotopeArea();
        totalHydrotopeArea += hydrotopeArea;
      }

      /* compare with area of catchment */
      final String checkMsg = checkCatchmentArea( catchment, totalHydrotopeArea );
      if( checkMsg != null )
        log.add( IStatus.WARNING, checkMsg );
    }
  }

  private String checkCatchmentArea( final Catchment catchment, final double hydrotopeArea )
  {
    final GM_Polygon geometry = catchment.getGeometry();
    final double catchmentArea = geometry.getArea();

    final double fehler = Math.abs( catchmentArea - hydrotopeArea );
    final double fehlerinProzent = 100.0 * fehler / hydrotopeArea;
    if( fehlerinProzent > 1.0 )
      return Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.3", hydrotopeArea, catchment.getName(), catchmentArea, fehler, fehlerinProzent ); //$NON-NLS-1$

    return null;
  }

  public DissolvedCatchment[] getDissolvedInfos( final Catchment catchment )
  {
    final Collection<DissolvedCatchment> elements = m_catchmentHash.get( catchment );
    return elements.toArray( new DissolvedCatchment[elements.size()] );
  }
}