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
package org.kalypso.ogc.gml.outline.handler;

import java.io.File;
import java.net.URLEncoder;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class ExportGMLThemeHandler extends AbstractHandler implements IHandler
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
    final String title = "GML-Export";

    final IStructuredSelection sel = (IStructuredSelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );

    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) sel.getFirstElement();
    final FeatureList featureList = theme == null ? null : theme.getFeatureList();
    if( featureList == null )
    {
      MessageDialog.openWarning( shell, title, "Kein Thema gew�hlt oder Thema enth�lt keine Daten." );
      return Status.CANCEL_STATUS;
    }

    /* ask user for file */
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoGisPlugin.getDefault(), "gmlExport" );
    final String lastDirPath = dialogSettings.get( SETTINGS_LAST_DIR );
    final FileDialog fileDialog = new FileDialog( shell, SWT.SAVE );
    fileDialog.setFilterExtensions( new String[] { "*.gml" } );
    fileDialog.setFilterNames( new String[] { "GML-Datei (*.gml)" } );
    fileDialog.setText( title );
    if( lastDirPath != null )
    {
      fileDialog.setFilterPath( lastDirPath );
      fileDialog.setFileName( theme.getLabel() );
    }
    else
      fileDialog.setFileName( theme.getLabel() );
    final String result = fileDialog.open();
    final File gmlFile;
    final File xsdFile;
    if( result == null )
    {
      gmlFile = null;
      xsdFile = null;
      return Status.CANCEL_STATUS;
    }
    else
    {
      gmlFile = new File( result );
      xsdFile = new File( FileUtilities.setSuffix( result, ".xsd" ) );
    }

    dialogSettings.put( SETTINGS_LAST_DIR, gmlFile.getParent() );

    final Job job = new Job( title + " - " + result )
    {
      @SuppressWarnings("unchecked")
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          final GMLWorkspace workspace = theme.getWorkspace();
          final IGMLSchema schema = workspace.getGMLSchema();
          final String encode = URLEncoder.encode( xsdFile.getName(), "UTF-8" );
          workspace.setSchemaLocation( schema.getTargetNamespace() + " " + encode );
          GmlSerializer.serializeWorkspace( gmlFile, workspace, "UTF-8" );
          if( schema instanceof GMLSchema )
          {
            // can only save real GMLSchema, EmptyGMLSchema is not saved
            ((GMLSchema) schema).getSchema().save( xsdFile );
          }
        }
        catch( final Exception e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }

        return Status.OK_STATUS;
      }
    };
    job.setUser( true );
    job.schedule();

    return Status.OK_STATUS;
  }
}