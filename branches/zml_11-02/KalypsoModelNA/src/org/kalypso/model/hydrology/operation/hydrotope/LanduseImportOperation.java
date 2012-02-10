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

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.suds.AbstractSud;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
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

    double getSealingCorrectionFactor( int index ) throws CoreException;

    String getDrainageType( int index ) throws CoreException;

    AbstractSud[] getSuds( int index ) throws CoreException;
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
    final SubMonitor progess = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation.0" ), size + 10 ); //$NON-NLS-1$

    if( m_importType == ImportType.CLEAR_OUTPUT )
    {
      final IFeatureBindingCollection<Landuse> landuses = m_output.getLanduses();
      landuses.clear();
    }

    ProgressUtilities.worked( progess, 10 );

    final IGMLSchema schema = m_output.getWorkspace().getGMLSchema();
    final IFeatureType lcFT = schema.getFeatureType( new QName( NaModelConstants.NS_NAPARAMETER, "Landuse" ) ); //$NON-NLS-1$
    final IRelationType pt = (IRelationType) schema.getFeatureType( Landuse.QNAME ).getProperty( Landuse.QNAME_PROP_LANDUSE );

    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );
    // traverse input workspace and import all single input landuses, if the landuse class exists
    for( int i = 0; i < size; i++ )
    {
      try
      {
        final String label = m_inputDescriptor.getName( i );
        final GM_MultiSurface geometry = m_inputDescriptor.getGeometry( i );
        if( geometry == null )
        {
          final String message = Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation.3", label ); //$NON-NLS-1$
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

        // find landuse-class
        final String landuseclass = m_inputDescriptor.getLanduseclass( i );

        // if there is no landuse class, we just update the original landuses with suds information
        if( landuseclass == null )
        {
          final AbstractSud[] suds = m_inputDescriptor.getSuds( i );

          // Handle existing landuses that intersect the new one
          final List<Landuse> existingLanduses = m_output.getLanduses().query( geometry.getEnvelope() );
          for( final Landuse landuse : existingLanduses )
          {
            /* add sud members */
            final IFeatureBindingCollection<Feature> sudCollection = landuse.getSudCollection();
            final GMLWorkspace landuseWorkspace = landuse.getWorkspace();
            final IGMLSchema landuseSchmea = landuseWorkspace.getGMLSchema();

            for( final Feature sud : suds )
            {
              final IRelationType rt = (IRelationType) landuseSchmea.getFeatureType( Landuse.QNAME_PROP_SUD_MEMBERS );
              final IFeatureType ft = sud.getFeatureType();
              final String href = String.format( "suds.gml#%s", sud.getId() ); //$NON-NLS-1$

              final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( landuse, rt, ft, href, null, null, null, null, null );
              sudCollection.add( lnk );
            }
          }
        }
        else
        {
          final Landuse landuse = m_output.importLanduse( label, geometry, m_importType, log );
          if( landuse != null )
          {
            final String desc = m_inputDescriptor.getDescription( i );
            final double corrSealing = m_inputDescriptor.getSealingCorrectionFactor( i );
            final String drainageType = m_inputDescriptor.getDrainageType( i );
            final AbstractSud[] suds = m_inputDescriptor.getSuds( i );

            landuse.setDescription( desc );
            landuse.setCorrSealing( corrSealing );
            landuse.setDrainageType( drainageType );

            /* add sud members */
            final IFeatureBindingCollection<Feature> sudCollection = landuse.getSudCollection();
            final GMLWorkspace landuseWorkspace = landuse.getWorkspace();
            final IGMLSchema landuseSchmea = landuseWorkspace.getGMLSchema();

            for( final Feature sud : suds )
            {
              final IRelationType rt = (IRelationType) landuseSchmea.getFeatureType( Landuse.QNAME_PROP_SUD_MEMBERS );
              final IFeatureType ft = sud.getFeatureType();
              final String href = String.format( "suds.gml#%s", sud.getId() ); //$NON-NLS-1$

              final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( landuse, rt, ft, href, null, null, null, null, null );
              sudCollection.add( lnk );
            }

            final String landuseRef = m_landuseClasses.getReference( landuseclass );
            if( landuseRef == null )
            {
              final String message = Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation.2", landuseclass, i + 1 ); //$NON-NLS-1$
              throw new CoreException( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
            }

            final String href = "parameter.gml#" + landuseRef; //$NON-NLS-1$
            final XLinkedFeature_Impl landuseXLink = new XLinkedFeature_Impl( landuse, pt, lcFT, href, null, null, null, null, null );

            landuse.setLanduse( landuseXLink );
          }
        }
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }

      ProgressUtilities.worked( progess, 1 );
    }

    if( !log.isEmpty() )
    {
      return new MultiStatus( "org.kalypso.NACalcJob", -1, log.toArray( new IStatus[] {} ), Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation.1" ), null ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    return Status.OK_STATUS;
  }

}
