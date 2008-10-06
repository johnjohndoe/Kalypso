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
package org.kalypso.ogc.gml.outline.handler;

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.serialize.Gml2ShapeConverter;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.IShapeDataProvider;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.TriangulatedSurfaceSinglePartShapeDataProvider;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;

/**
 * @author Gernot Belger
 */
public class ExportGml2ShapeThemeHandler extends AbstractHandler implements IHandler
{

  private static final String SETTINGS_LAST_DIR = "lastDir";

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    if( part == null )
      throw new ExecutionException( "No active part." );

    final Shell shell = part.getSite().getShell();
    final String title = "Shape-Export";

    final IStructuredSelection sel = (IStructuredSelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );

    final Object[] array = sel.toArray();

    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) sel.getFirstElement();
    final FeatureList featureList = theme == null ? null : theme.getFeatureList();
    if( featureList == null )
    {
      MessageDialog.openWarning( shell, title, "Kein Thema gew‰hlt oder Thema enth‰lt keine Daten." );
      return Status.CANCEL_STATUS;
    }

    final Gml2ShapeConverter converter = Gml2ShapeConverter.createDefault( theme.getFeatureType() );

    // examine what we got and ask user
    // TODO: only use file extension which make sense (dbf OR shp)

    /* ask user for file */
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoGisPlugin.getDefault(), "gml2shapeExport" );
    final String lastDirPath = dialogSettings.get( SETTINGS_LAST_DIR );
    final FileDialog fileDialog = new FileDialog( shell, SWT.SAVE );
    fileDialog.setFilterExtensions( new String[] { "*.*", "*.shp", "*.dbf" } );
    fileDialog.setFilterNames( new String[] { "Alle Dateien (*.*)", "ESRI Shape (*.shp)", "DBase (*.dbf)" } );
    fileDialog.setText( title );
    if( lastDirPath != null )
    {
      fileDialog.setFilterPath( lastDirPath );
      fileDialog.setFileName( theme.getLabel() );
    }
    else
      fileDialog.setFileName( theme.getLabel() );
    final String result = fileDialog.open();
    if( result == null )
      return Status.CANCEL_STATUS;

    dialogSettings.put( SETTINGS_LAST_DIR, new File( result ).getParent() );

    final String shapeFileBase;
    if( result.toLowerCase().endsWith( ".shp" ) || result.toLowerCase().endsWith( ".dbf" ) )
      shapeFileBase = FileUtilities.nameWithoutExtension( result );
    else
      shapeFileBase = result;

    final Job job = new Job( title + " - " + result )
    {
      @SuppressWarnings("unchecked")
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        IShapeDataProvider shapeDataProvider = null;

        Feature feature = (Feature) featureList.get( 0 );
        GM_Object geometryProperty = feature.getDefaultGeometryProperty();
        if( geometryProperty instanceof GM_TriangulatedSurface_Impl )
        {
          final byte shapeType = ShapeConst.SHAPE_TYPE_POLYGONZ;
          shapeDataProvider = new TriangulatedSurfaceSinglePartShapeDataProvider( (Feature[]) featureList.toArray( new Feature[featureList.size()] ), shapeType );
        }
        try
        {
          converter.writeShape( featureList, shapeFileBase, shapeDataProvider, monitor );
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
        return Status.OK_STATUS;
      }
    };
    job.setUser( true );
    job.schedule();

    return Status.OK_STATUS;
  }
}