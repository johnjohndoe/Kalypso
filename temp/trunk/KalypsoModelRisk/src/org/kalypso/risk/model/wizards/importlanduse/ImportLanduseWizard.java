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
package org.kalypso.risk.model.wizards.importlanduse;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.opengis.cs.CS_CoordinateSystem;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportLanduseWizard extends Wizard implements INewWizard
{
  private IStructuredSelection initialSelection;

  private IFolder m_scenarioFolder;

  protected ImportLanduseShpPage m_PageImportShp;

  private final HashSet<String> m_landuseTypeSet = new HashSet<String>();

  public ImportLanduseWizard( )
  {
    super();
  }

  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    m_scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "ImportLanduseWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_PageImportShp = new ImportLanduseShpPage();
    m_PageImportShp.init( initialSelection );
    addPage( m_PageImportShp );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return m_PageImportShp.isPageComplete();
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final String landuseProperty = m_PageImportShp.getLanduseProperty();
    final String sourceShapeFilePath = m_PageImportShp.getSourceLocation().removeFileExtension().toPortableString();
    final CS_CoordinateSystem coordinateSystem = m_PageImportShp.getCoordinateSystem();
    try
    {
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor ) throws InterruptedException
        {
          monitor.beginTask( Messages.getString( "ImportLanduseWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
          try
          {
            // final IWorkbench workbench = UIPlugin.getDefault().getWorkbench();
            // final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
            // final IEvaluationContext context = handlerService.getCurrentState();
            // final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable(
            // ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
            // final ILandusePolygonCollection model = new LandusePolygonCollection(
            // getFeature().getWorkspace().getRootFeature() );

            final IFile landuseDataResource = m_scenarioFolder.getFile( "/models/LanduseVectorData.gml" ); //$NON-NLS-1$
            final QName shapeGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
            final QName shapeLandusePropertyName = new QName( "namespace", landuseProperty ); //$NON-NLS-1$

            final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

            // Comment: probably a good idea to wait for the map before accessing the data
            // not nessesary, because loadin the model from the pool is synchronized with the map loading
            
            monitor.subTask( "Loading workspaces" );
            final URL landuseDataURL = ResourceUtilities.createURL( landuseDataResource );
            final PoolableObjectType key = new PoolableObjectType( "gml", landuseDataURL.toExternalForm(), landuseDataURL );
            final CommandableWorkspace landuseVectorWS = (CommandableWorkspace) pool.getObject( key );

            final IFeatureType landusePolygonFeatureType = landuseVectorWS.getGMLSchema().getFeatureType( ILandusePolygon.QNAME );
            final Feature landuseCollection = landuseVectorWS.getRootFeature();

            final GMLWorkspace landuseShapeWS = ShapeSerializer.deserialize( sourceShapeFilePath, coordinateSystem );

            final Feature shapeRootFeature = landuseShapeWS.getRootFeature();
            final List shapeFeatureList = (List) shapeRootFeature.getProperty( new QName( "namespace", Messages.getString( "ImportLanduseWizard.3" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
            
            final List polygonMembers = (List) landuseCollection.getProperty( ILandusePolygonCollection.QNAME_PROPERTY_POLYGON );
            polygonMembers.clear();
            
            final IRelationType polygonRelationType = (IRelationType) landuseCollection.getFeatureType().getProperty( ILandusePolygonCollection.QNAME_PROPERTY_POLYGON );
            final List<Feature> changedFeatures = new ArrayList<Feature>();
            
            monitor.subTask( "Importing data" );
            for( int i = 0; i < shapeFeatureList.size(); i++ )
            {
              final Feature shpFeature = (Feature) shapeFeatureList.get( i );
              final String shpPropertyValue = (String) shpFeature.getProperty( shapeLandusePropertyName );
              if( !m_landuseTypeSet.contains( shpPropertyValue ) )
                m_landuseTypeSet.add( shpPropertyValue );
              final Feature feature = landuseVectorWS.createFeature( landuseCollection, polygonRelationType, landusePolygonFeatureType );
              changedFeatures.add( feature );
              final GM_Object shpGeometryProperty = (GM_Object) shpFeature.getProperty( shapeGeomPropertyName );

              // we don't like multi surfaces, so...
              if( shpGeometryProperty instanceof GM_MultiSurface )
              {
                final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) shpGeometryProperty).clone();
                final GM_Surface< ? >[] surfaces = multiSurface.getAllSurfaces();
                for( int k = 0; k < surfaces.length; k++ )
                {
                  feature.setProperty( ILandusePolygon.QNAME_PROPERTY_GEOMETRY, surfaces[k] );
                  feature.setProperty( ILandusePolygon.QNAME_PROPERTY_SLDSTYLE, shpPropertyValue );
                  FeatureHelper.addProperty( landuseCollection, polygonRelationType, feature );
                }
              }
              else if( shpGeometryProperty instanceof GM_Surface )
              {
                feature.setProperty( ILandusePolygon.QNAME_PROPERTY_GEOMETRY, shpGeometryProperty );
                feature.setProperty( ILandusePolygon.QNAME_PROPERTY_SLDSTYLE, shpPropertyValue );
                FeatureHelper.addProperty( landuseCollection, polygonRelationType, feature );
              }
              else
                throw new RuntimeException( Messages.getString( "ImportLanduseWizard.4" ) + shpGeometryProperty.getClass().getName() ); //$NON-NLS-1$
            }
            landuseVectorWS.fireModellEvent( new FeatureStructureChangeModellEvent( landuseVectorWS, landuseCollection, changedFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            // GmlSerializer.serializeWorkspace( landuseDataFile, landuseVectorWS, "UTF-8" ); //$NON-NLS-1$
            // landuseDataResource.refreshLocal( IFile.DEPTH_ZERO, new NullProgressMonitor() );

            // send an (empty?) command to the workspace in order to make the pool dirty
            landuseVectorWS.postCommand( new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

            // TODO create landuse class database
            monitor.subTask( "Creating landuse database" );
            
            
          }
          catch( final Exception e )
          {
            throw new InterruptedException( e.getLocalizedMessage() );
          }
        }
      } );
    }
    catch( final Exception e )
    {
      ErrorDialog.openError( getShell(), Messages.getString( "ImportLanduseWizard.6" ), "", StatusUtilities.statusFromThrowable( e ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
      return false;
    }
    return true;
  }

}
