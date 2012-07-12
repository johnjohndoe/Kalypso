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
package org.kalypso.ui.rrm.internal.simulations.actions;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.StatusCollection;

/**
 * This action opens a status log.
 * 
 * @author Holger Albert
 */
public class OpenStatusLogAction extends Action implements IUpdateable
{
  /**
   * The status log file.
   */
  private final IFile m_statusLogFile;

  /**
   * @param text
   *          The text.
   * @param tooltipText
   *          The tooltip text.
   * @param statusLogFile
   *          The status log file.
   */
  public OpenStatusLogAction( final String text, final String tooltipText, final IFile statusLogFile )
  {
    super( text );

    setToolTipText( tooltipText );

    m_statusLogFile = statusLogFile;
  }

  @Override
  public void run( )
  {
    try
    {
      /* Get the file. */
      final File statusLogFile = m_statusLogFile.getLocation().toFile();

      /* Check if the status log file exists. */
      if( !statusLogFile.exists() )
        throw new IOException( String.format( "The status log file '%s' does not exist...", statusLogFile.getName() ) );

      /* Read the status log. */
      final IStatus statusLog = readStatusLog();

      /* If only one simulation was calculated, descend on level. */
      IStatus statusToSet = statusLog;
      if( statusLog.isMultiStatus() )
      {
        final IStatus[] children = statusLog.getChildren();
        if( children.length == 1 )
          statusToSet = children[0];
      }

      /* Always show dialog. */
      final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final String dialogTitle = getText();
      final StatusDialog statusDialog = new StatusDialog( shell, statusToSet, dialogTitle );
      statusDialog.open();
    }
    catch( final Exception ex )
    {
      /* Display the error. */
      final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final String dialogTitle = getText();
      final String message = "The file could not be opened...";
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex );
      ErrorDialog.openError( shell, dialogTitle, message, status );
    }
  }

  @Override
  public ImageDescriptor getImageDescriptor( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.OPEN_STATUS_LOG_ACTION );
  }

  @Override
  public void update( )
  {
    if( m_statusLogFile.exists() )
      setEnabled( true );
    else
      setEnabled( false );
  }

  /**
   * This function reads the status log and returns it.
   * 
   * @return The status log.
   */
  private IStatus readStatusLog( )
  {
    try
    {
      final StatusCollector results = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_statusLogFile );
      final Feature rootFeature = workspace.getRootFeature();
      final StatusCollection statusCollection = (StatusCollection) rootFeature;
      for( final IGeoStatus geoStatus : statusCollection.getStati() )
        results.add( geoStatus );

      return results.asMultiStatus( "Status log" );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Reading the status log has failed.", ex );
    }
  }
}