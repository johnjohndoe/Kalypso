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
package org.kalypso.ogc.gml.om.table.command;

import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.i18n.Messages;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.LastLineContentProvider;
import org.kalypso.ogc.gml.om.table.LastLineLabelProvider;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiFirstColumnHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;

/**
 * Pastes the contents of the clipboard to the TupleResult
 * 
 * @author Dejan Antanaskovic
 */
public class PasteFromClipboardHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    String trstring = null;
    try
    {
      trstring = (String) (Toolkit.getDefaultToolkit().getSystemClipboard().getContents( this ).getTransferData( DataFlavor.stringFlavor ));
      // if Cipboard content is not text or that content is empty, just ignore it
      if( trstring == null || trstring.trim().length() == 0 )
      {
        MessageDialog.openError( shell, Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.0" ), Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }
    }
    catch( Exception e )
    {
      MessageDialog.openError( shell, Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.2" ), Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return null;
    }

    try
    {
      final TableViewer tupleResultViewer = TupleResultCommandUtils.findTableViewer( event );
      if( tupleResultViewer == null )
      {
        MessageDialog.openError( shell, Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.4" ), Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }
      final IContentProvider contentProvider = tupleResultViewer.getContentProvider();
      TupleResultContentProvider resultContentProvider = null;
      if( contentProvider instanceof TupleResultContentProvider )
        resultContentProvider = (TupleResultContentProvider) contentProvider;
      else if( contentProvider instanceof LastLineContentProvider )
      {
        final IStructuredContentProvider wrappedProvider = ((LastLineContentProvider) contentProvider).getWrappedProvider();
        if( wrappedProvider instanceof TupleResultContentProvider )
          resultContentProvider = (TupleResultContentProvider) wrappedProvider;
      }
      if( resultContentProvider == null )
        return null;

      final TupleResult tupleResult = TupleResultCommandUtils.findTupleResult( event );

      final List<IRecord> addedRecords = new ArrayList<IRecord>();

      // TODO: is this necessary? why not just replace the selected stuff or similar?
// tupleResult.clear();
      IStructuredSelection selection = (IStructuredSelection) tupleResultViewer.getSelection();
      Object firstElement = selection.getFirstElement();
      int addIndex;
      if( firstElement == null )
        addIndex = tupleResult.size();
      else
        addIndex = tupleResult.indexOf( firstElement ) + 1;

      final StringTokenizer st1 = new StringTokenizer( trstring, "\n" ); //$NON-NLS-1$
      while( st1.hasMoreTokens() )
      {
        final String line = st1.nextToken();
        if( line.startsWith( LastLineLabelProvider.DUMMY_ELEMENT_TEXT ) )
          break;
        final StringTokenizer tokens = new StringTokenizer( line, "\t" ); //$NON-NLS-1$
        final IRecord record = tupleResult.createRecord();

        final Object[] columnProperties = tupleResultViewer.getColumnProperties();

        for( Object columnProperty : columnProperties )
        {
          IComponentUiHandler handler = resultContentProvider.getHandler( columnProperty.toString() );

          if( handler instanceof ComponentUiFirstColumnHandler )
            continue;

          if( !tokens.hasMoreElements() )
            continue;

          final Object value = handler.parseValue( tokens.nextToken() );
          handler.setValue( record, value );
        }

        tupleResult.add( addIndex++, record );

        addedRecords.add( record );
      }

      new UIJob( "" )
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          tupleResultViewer.setSelection( new StructuredSelection( addedRecords ), true );
          return Status.OK_STATUS;
        }
      }.schedule();
    }
    catch( final Exception ex )
    {
      MessageDialog.openError( shell, Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.12" ), Messages.getString( "org.kalypso.ogc.gml.om.table.command.PasteFromClipboardHandler.13" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      ex.printStackTrace();
    }
    return null;
  }
}
