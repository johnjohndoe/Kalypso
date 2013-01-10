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
package org.kalypso.model.hydrology.operation.hydrotope;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * Imports landuse into a 'landuse.gml' file from another gml-workspace (probably a shape-file).
 * 
 * @author Gernot Belger
 */
public class LanduseImportOperation extends AbstractImportOperation<GM_MultiSurface>
{
  public interface InputDescriptor extends AbstractImportOperation.InputDescriptor<GM_MultiSurface>
  {
    String getLanduseclass( int index ) throws CoreException;

    double getSealingCorrectionFactor( int index ) throws CoreException;
  }

  private final LanduseCollection m_output;

  private final ImportType m_importType;

  private final InputDescriptor m_inputDescriptor;

  private final ILanduseClassDelegate m_landuseClasses;

  /**
   * @param output
   *          An (empty) list containing rrmLanduse:landuse features
   */
  public LanduseImportOperation( final InputDescriptor inputDescriptor, final LanduseCollection output, final ILanduseClassDelegate landuseClasses, final ImportType importType )
  {
    super( inputDescriptor );

    m_inputDescriptor = inputDescriptor;
    m_output = output;
    m_landuseClasses = landuseClasses;
    m_importType = importType;
  }

  @Override
  protected void init( )
  {
    final IFeatureBindingCollection<Landuse> landusesOut = m_output.getLanduses();
    if( m_importType == ImportType.CLEAR_OUTPUT )
    {
      final IFeatureBindingCollection<Landuse> landuses = landusesOut;
      landuses.clear();
    }
  }

  @Override
  protected Feature importRow( final int i, final String label, final GM_MultiSurface geometry, final IStatusCollector log ) throws CoreException
  {
    // find landuse-class
    final String landuseclass = m_inputDescriptor.getLanduseclass( i );

    // if there is no landuse class, we just update the original landuses with suds information
    if( landuseclass == null )
    {
      // FIXME: is this a hack just for Planer-Client?? In that case -> do error handling in case of 'normal' landuse
      // import, else the user never gets an error message here...!

      return m_output.importLanduse( ImportType.UPDATE, label, geometry, null, null );
    }
    else
    {
      final double corrSealing = m_inputDescriptor.getSealingCorrectionFactor( i );
      final String landuseRef = m_landuseClasses.getReference( landuseclass );
      if( landuseRef == null )
      {
        final String message = Messages.getString( "org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.2", landuseclass, i + 1 ); //$NON-NLS-1$
        throw new CoreException( new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, message ) );
      }

      return m_output.importLanduse( m_importType, label, geometry, corrSealing, landuseRef );
    }
  }
}