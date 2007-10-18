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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.internal.UIPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILanduseModel;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.opengis.cs.CS_CoordinateSystem;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportLanduseWizard extends Wizard implements INewWizard
{
  private IStructuredSelection initialSelection;

  protected ImportLanduseShpPage m_PageImportShp;

  private final HashSet<String> m_landuseTypeSet = new HashSet<String>();

  public ImportLanduseWizard( )
  {
    super();
  }

  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
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
            monitor.subTask( Messages.getString( "ImportLanduseWizard.7" ) ); //$NON-NLS-1$
            final IWorkbench workbench = UIPlugin.getDefault().getWorkbench();
            final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
            final IEvaluationContext context = handlerService.getCurrentState();
            final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
            final ILanduseModel landuseModel = szenarioDataProvider.getModel( ILanduseModel.class );

            final QName shapeGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
            final QName shapeLandusePropertyName = new QName( "namespace", landuseProperty ); //$NON-NLS-1$

            final GMLWorkspace landuseShapeWS = ShapeSerializer.deserialize( sourceShapeFilePath, coordinateSystem );

            final Feature shapeRootFeature = landuseShapeWS.getRootFeature();
            final List shapeFeatureList = (List) shapeRootFeature.getProperty( new QName( "namespace", Messages.getString( "ImportLanduseWizard.3" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$

            final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection = landuseModel.getLandusePolygonCollection();
            landusePolygonCollection.clear();

            monitor.subTask( Messages.getString( "ImportLanduseWizard.9" ) ); //$NON-NLS-1$
            final List<Feature> createdFeatures = new ArrayList<Feature>();
            for( int i = 0; i < shapeFeatureList.size(); i++ )
            {
              final Feature shpFeature = (Feature) shapeFeatureList.get( i );
              final String shpPropertyValue = shpFeature.getProperty( shapeLandusePropertyName ).toString();
              if( !m_landuseTypeSet.contains( shpPropertyValue ) )
                m_landuseTypeSet.add( shpPropertyValue );
              final ILandusePolygon polygon = landusePolygonCollection.addNew( ILandusePolygon.QNAME );
              final GM_Object shpGeometryProperty = (GM_Object) shpFeature.getProperty( shapeGeomPropertyName );

              // we don't like multi surfaces, so...
              if( shpGeometryProperty instanceof GM_MultiSurface )
              {
                final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) shpGeometryProperty).clone();
                final GM_Surface< ? >[] surfaces = multiSurface.getAllSurfaces();
                for( int k = 0; k < surfaces.length; k++ )
                {
                  polygon.setGeometry( surfaces[k] );
                  polygon.setStyleType( shpPropertyValue );
                  polygon.getWrappedFeature().invalidEnvelope();
                  createdFeatures.add( polygon.getWrappedFeature() );
                }
              }
              else if( shpGeometryProperty instanceof GM_Surface )
              {
                polygon.setGeometry( (GM_Surface< ? >) shpGeometryProperty );
                polygon.setStyleType( shpPropertyValue );
                polygon.getWrappedFeature().invalidEnvelope();
                createdFeatures.add( polygon.getWrappedFeature() );
              }
              else
                throw new RuntimeException( Messages.getString( "ImportLanduseWizard.4" ) + shpGeometryProperty.getClass().getName() ); //$NON-NLS-1$
            }
            // landuseModel.getWrappedFeature().invalidEnvelope();

            // fireModellEvent to redraw a map...
            final GMLWorkspace workspace = szenarioDataProvider.getCommandableWorkSpace( ILanduseModel.class );
            workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, landusePolygonCollection.getWrappedFeature(), createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

            szenarioDataProvider.postCommand( ILanduseModel.class, new EmptyCommand( "Get dirty!", false ) );

            // TODO create landuse class database
            monitor.subTask( Messages.getString( "ImportLanduseWizard.10" ) ); //$NON-NLS-1$

            final IRasterizationControlModel controlModel = szenarioDataProvider.getModel( IRasterizationControlModel.class );
            final IFeatureWrapperCollection<ILanduseClass> landuseClassCollection = controlModel.getLanduseClassCollection();
            landuseClassCollection.clear();
            for( final String landuseType : m_landuseTypeSet )
            {
              final ILanduseClass landuseClass = landuseClassCollection.addNew( ILanduseClass.QNAME );
              landuseClass.setName( landuseType );
              landuseClass.setColorStyle( getRandomColor() );
            }
            szenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Get dirty!", false ) );

            final IFolder projectFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
            final IFile sldFile = projectFolder.getFile( "styles/LanduseVectorModel.sld" );
            if( sldFile.exists() )
              sldFile.delete( false, new NullProgressMonitor() );
            SLDHelper.exportSLD( sldFile, landuseClassCollection, ILandusePolygon.QNAME_PROPERTY_GEOMETRY, ILandusePolygon.QNAME_PROPERTY_SLDSTYLE, "Landuse style", "Landuse style", null );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
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

  private RGB getRandomColor( )
  {
    return new RGB( new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue() );
  }
}
