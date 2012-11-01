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

import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.SoilType;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * Imports pedology into a 'pedology.gml' file from another gml-workspace (probably a shape-file).
 * 
 * @author Gernot Belger, Dejan Antanaskovic
 */
public class PedologyImportOperation extends AbstractImportOperation<GM_MultiSurface>
{
  public interface InputDescriptor extends AbstractImportOperation.InputDescriptor<GM_MultiSurface>
  {
    String getSoilType( int index ) throws CoreException;
  }

  private final SoilTypeCollection m_output;

  private final ImportType m_importType;

  private final InputDescriptor m_inputDescriptor;

  private final Map<String, String> m_soilTypes;

  /**
   * @param output
   *          An (empty) list containing rrmsoilType:soilType features
   */
  public PedologyImportOperation( final InputDescriptor inputDescriptor, final SoilTypeCollection output, final Map<String, String> soilTypes, final ImportType importType )
  {
    super( inputDescriptor );

    m_inputDescriptor = inputDescriptor;
    m_output = output;
    m_soilTypes = soilTypes;
    m_importType = importType;
  }

  @Override
  protected void init( )
  {
    final IFeatureBindingCollection<SoilType> soilTypes = m_output.getSoilTypes();
    if( m_importType == ImportType.CLEAR_OUTPUT )
      soilTypes.clear();
  }

  @Override
  protected Feature importRow( final int i, final String label, final GM_MultiSurface geometry, final IStatusCollector log ) throws CoreException
  {
    final String soilTypeLink = m_inputDescriptor.getSoilType( i );

    // find soilType-class
    final String soilTypeRef = m_soilTypes.get( soilTypeLink );
    if( soilTypeRef == null )
    {
      final String message = Messages.getString( "org.kalypso.convert.namodel.hydrotope.PedologyImportOperation.2", soilTypeLink, i + 1 ); //$NON-NLS-1$
      throw new CoreException( new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, message ) );
    }

    final SoilType soilType = m_output.importSoilType( label, geometry, m_importType, log );
    if( soilType != null )
    {
      final String desc = m_inputDescriptor.getDescription( i );

      soilType.setDescription( desc );

      final String href = "parameter.gml#" + soilTypeRef; //$NON-NLS-1$

      soilType.setSoilType( href );
    }

    return soilType;
  }
}