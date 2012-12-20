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
package org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler;

import java.math.BigInteger;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.command.ToolbarCommandUtils;

public class Command1D2DTimestepsAddRow extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    try
    {
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
      final TableViewer viewer = ToolbarCommandUtils.findTableViewer( event );
      final TupleResult tupleResult = ToolbarCommandUtils.findTupleResult( event );
      if( viewer == null || tupleResult == null )
        throw new ExecutionException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsAddRow.0" ) ); //$NON-NLS-1$

      final IRecord row = tupleResult.createRecord();
      // if tuple result is empty or contains just a single entry, we will just add another row with no interpolation
      // default date/time is 01.01.2000 00:00:00.000
      if( tupleResult.size() < 2 )
      {
        final GregorianCalendar calendar = new GregorianCalendar( KalypsoCorePlugin.getDefault().getTimeZone() );
        calendar.set( GregorianCalendar.DAY_OF_MONTH, 1 );
        calendar.set( GregorianCalendar.MONTH, GregorianCalendar.JANUARY );
        calendar.set( GregorianCalendar.YEAR, 2000 );
        calendar.set( GregorianCalendar.HOUR_OF_DAY, 0 );
        calendar.set( GregorianCalendar.MINUTE, 0 );
        calendar.set( GregorianCalendar.SECOND, 0 );
        calendar.set( GregorianCalendar.MILLISECOND, 0 );
        XMLGregorianCalendar xmlCalendar = DateUtilities.toXMLGregorianCalendar( calendar );
        row.setValue( 1, xmlCalendar );
        // changed to string to allow more flexible expansion of "Relaxation Factor"
        row.setValue( 2, new String( "1.0" ) ); //$NON-NLS-1$
        tupleResult.add( row );
      }
      else
      {
        final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
        if( selection == null || selection.size() == 0 )
        {
          MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsAddRow.1" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsAddRow.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          throw new ExecutionException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsAddRow.3" ) ); //$NON-NLS-1$
        }

        final int index = tupleResult.indexOf( selection.getFirstElement() );

        final IRecord previous, next;
        final long timeMillis;
        final TimeZone timeZone;
        if( index == tupleResult.size() - 1 )
        {
          // last record is selected, special case
          previous = tupleResult.get( index - 1 );
          next = tupleResult.get( index );
          final long t1 = ((XMLGregorianCalendar) previous.getValue( 1 )).toGregorianCalendar().getTimeInMillis();
          final long t2 = ((XMLGregorianCalendar) next.getValue( 1 )).toGregorianCalendar().getTimeInMillis();
          timeMillis = t2 + (t2 - t1);
          timeZone = ((XMLGregorianCalendar) previous.getValue( 1 )).toGregorianCalendar().getTimeZone();
        }
        else
        {
          previous = tupleResult.get( index );
          next = tupleResult.get( index + 1 );
          final long t1 = ((XMLGregorianCalendar) previous.getValue( 1 )).toGregorianCalendar().getTimeInMillis();
          final long t2 = ((XMLGregorianCalendar) next.getValue( 1 )).toGregorianCalendar().getTimeInMillis();
          timeMillis = t1 + (t2 - t1) / 2;
          timeZone = ((XMLGregorianCalendar) previous.getValue( 1 )).toGregorianCalendar().getTimeZone();
        }

        final GregorianCalendar calendar = new GregorianCalendar( timeZone );
        calendar.setTimeInMillis( timeMillis );
        row.setValue( 0, new BigInteger( "0" ) ); //$NON-NLS-1$
        XMLGregorianCalendar xmlCalendar = DateUtilities.toXMLGregorianCalendar( calendar );
        row.setValue( 1, xmlCalendar );
        row.setValue( 2, previous.getValue( 2 ) );
        tupleResult.add( index + 1, row );
      }
      // select the new row; in ui job, as table is also updated in an ui event
      new UIJob( "" ) //$NON-NLS-1$
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          viewer.setSelection( new StructuredSelection( row ) );
          return Status.OK_STATUS;
        }
      }.schedule();

      return null;
    }
    catch( ExecutionException e )
    {
      throw e;
    }
    catch( Exception e )
    {
      throw new ExecutionException( e.getLocalizedMessage(), e );
    }
  }
}