/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.convert.namodel.hydrotope;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.SoilType;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Imports pedology into a 'pedology.gml' file from another gml-workspace (probably a shape-file).
 * 
 * @author Gernot Belger, Dejan Antanaskovic
 */
public class PedologyImportOperation implements ICoreRunnableWithProgress
{
  public static interface InputDescriptor
  {
    /** Number of elements contained in this descriptor. All other methods allow for indices in the range 0..size-1 */
    int size( ) throws CoreException;

    String getName( int index );

    String getDescription( int index );

    GM_MultiSurface getGeometry( int index ) throws CoreException;

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
    m_inputDescriptor = inputDescriptor;
    m_output = output;
    m_soilTypes = soilTypes;
    m_importType = importType;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final int size = m_inputDescriptor.size();
    final SubMonitor progess = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.convert.namodel.hydrotope.PedologyImportOperation.0" ), size + 10 ); //$NON-NLS-1$

    final IFeatureBindingCollection<SoilType> soilTypes = m_output.getSoilTypes();
    if( m_importType == ImportType.CLEAR_OUTPUT )
      soilTypes.clear();

    ProgressUtilities.worked( progess, 10 );

    final IGMLSchema schema = m_output.getWorkspace().getGMLSchema();
    final IFeatureType lcFT = schema.getFeatureType( new QName( NaModelConstants.NS_NAPARAMETER, "soilType" ) ); //$NON-NLS-1$
    final IRelationType pt = (IRelationType) schema.getFeatureType( SoilType.QNAME ).getProperty( SoilType.QNAME_PROP_SOILTYPE );

    final List<IStatus> log = new ArrayList<IStatus>();
    // traverse input workspace and import all single input soilTypes, if the soilType class exists
    for( int i = 0; i < size; i++ )
    {
      try
      {
        final String label = m_inputDescriptor.getName( i );
        final GM_MultiSurface geometry = m_inputDescriptor.getGeometry( i );
        final String soilTypeLink = m_inputDescriptor.getSoilType( i );

        // find soilType-class
        final String soilTypeRef = m_soilTypes.get( soilTypeLink );
        if( soilTypeRef == null )
        {
          final String message = Messages.getString( "org.kalypso.convert.namodel.hydrotope.PedologyImportOperation.2", soilTypeLink, i + 1 ); //$NON-NLS-1$
          throw new CoreException( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }

        if( geometry == null )
        {
          final String message = Messages.getString( "org.kalypso.convert.namodel.hydrotope.PedologyImportOperation.3", label ); //$NON-NLS-1$
          log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }
        else
        {
          final IStatus isValidTop = TopologyChecker.checkTopology( geometry, label );
          if( !isValidTop.isOK() )
          {
            log.add( isValidTop );
          }
        }

        final SoilType soilType = m_output.importSoilType( label, geometry, m_importType, log );
        if( soilType != null )
        {
          final String desc = m_inputDescriptor.getDescription( i );

          soilType.setDescription( desc );

          final String href = "parameter.gml#" + soilTypeRef; //$NON-NLS-1$
          final XLinkedFeature_Impl soilTypeXLink = new XLinkedFeature_Impl( soilType, pt, lcFT, href, null, null, null, null, null );

          soilType.setSoilType( soilTypeXLink );
        }
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }

      ProgressUtilities.worked( progess, 1 );
    }

    return Status.OK_STATUS;
  }

}
