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
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.schema.binding.Landuse;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection.ImportType;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Imports landuse into a 'landuse.gml' file from another gml-workspace (probably a shape-file).
 *
 * @author Gernot Belger
 */
public class LanduseImportOperation implements ICoreRunnableWithProgress
{
  public static interface InputDescriptor
  {
    /** Number of elements contained in this descriptor. All other methods allow for indices in the range 0..size-1 */
    int size( ) throws CoreException;

    String getName( int index );

    String getDescription( int index );

    GM_MultiSurface getGeometry( int index ) throws CoreException;

    String getLanduseclass( int index ) throws CoreException;

    double getCorrSealing( int index ) throws CoreException;

    String getDrainageType( int index ) throws CoreException;
  }

  private final LanduseCollection m_output;

  private final ImportType m_importType;

  private final InputDescriptor m_inputDescriptor;

  private final Map<String, String> m_landuseClasses;

  /**
   * @param output
   *          An (empty) list containing rrmLanduse:landuse features
   */
  public LanduseImportOperation( final InputDescriptor inputDescriptor, final LanduseCollection output, final Map<String, String> landuseClasses, final ImportType importType )
  {
    m_inputDescriptor = inputDescriptor;
    m_output = output;
    m_landuseClasses = landuseClasses;
    m_importType = importType;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final int size = m_inputDescriptor.size();
    final SubMonitor progess = SubMonitor.convert( monitor, "Importing landuses", size + 10 );

    final IFeatureBindingCollection<Landuse> landuses = m_output.getLanduses();
    if( m_importType == ImportType.CLEAR_OUTPUT )
      landuses.clear();

    ProgressUtilities.worked( progess, 10 );

    final IGMLSchema schema = m_output.getWorkspace().getGMLSchema();
    final IFeatureType lcFT = schema.getFeatureType( new QName( NaModelConstants.NS_NAPARAMETER, "Landuse" ) );
    final IRelationType pt = (IRelationType) schema.getFeatureType( Landuse.QNAME ).getProperty( Landuse.QNAME_PROP_LANDUSE );

    final Map<String, String> landuseClasses = getLandusClasses();

    final List<IStatus> log = new ArrayList<IStatus>();
    // traverse input workspace and import all single input landuses, if the landuse class exists
    for( int i = 0; i < size; i++ )
    {
      try
      {
        final String label = m_inputDescriptor.getName( i );
        final GM_MultiSurface geometry = m_inputDescriptor.getGeometry( i );
        final String landuseclass = m_inputDescriptor.getLanduseclass( i );

        // find landuse-class
        final String landuseRef = landuseClasses.get( landuseclass );
        if( landuseRef == null )
        {
          final String message = String.format( "Unknwon landuse class '%s' at feature-id %d", landuseclass, i + 1 );
          throw new CoreException( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }

        if( geometry == null )
        {
          final String message = String.format( "Null geometry at feature: %s", label );
          log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }

        final Landuse landuse = m_output.importLanduse( label, geometry, m_importType, log );
        if( landuse != null )
        {
          final String desc = m_inputDescriptor.getDescription( i );
          final double corrSealing = m_inputDescriptor.getCorrSealing( i );
          final String drainageType = m_inputDescriptor.getDrainageType( i );

          landuse.setDescription( desc );
          landuse.setCorrSealing( corrSealing );
          landuse.setDrainageType( drainageType );

          final String href = "parameter.gml#" + landuseRef;
          final XLinkedFeature_Impl landuseXLink = new XLinkedFeature_Impl( landuse, pt, lcFT, href, null, null, null, null, null );

          landuse.setLanduse( landuseXLink );
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

  private Map<String, String> getLandusClasses( )
  {
    return m_landuseClasses;
  }

}
