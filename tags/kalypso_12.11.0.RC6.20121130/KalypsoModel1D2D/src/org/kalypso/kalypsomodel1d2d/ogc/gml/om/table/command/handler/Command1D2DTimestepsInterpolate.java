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
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
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
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.command.ToolbarCommandUtils;

/**
 * Pastes the contents of the clipboard to the TupleResult
 *
 * @author Dejan Antanaskovic
 */
public class Command1D2DTimestepsInterpolate extends AbstractHandler
{
  private IStructuredSelection m_selection;

  private TupleResult m_tupleResult;

  protected List<IRecord> m_resultRecords = new ArrayList<>();

  public static enum INTERPOLATION_METHOD
  {
    NUMBER_OF_STEPS,
    TIME_INTERVAL
  }

  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final TableViewer viewer = ToolbarCommandUtils.findTableViewer( event );
    m_tupleResult = ToolbarCommandUtils.findTupleResult( event );
    if( viewer == null || m_tupleResult == null )
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.0" ) ); //$NON-NLS-1$

    m_selection = (IStructuredSelection) viewer.getSelection();
    if( m_selection == null )
    {
      MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.1" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.3" ) ); //$NON-NLS-1$
    }

    if( m_selection.size() < 2 )
    {
      MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.4" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.6" ) ); //$NON-NLS-1$
    }
    final int index = m_tupleResult.indexOf( m_selection.getFirstElement() );
    // changed to string to allow more flexible expansion of "Relaxation Factor"
    final String relaxationFactor = (String) m_tupleResult.get( index ).getValue( 2 );

    final Command1D2DTimestepsInterpolationWizard wizard = new Command1D2DTimestepsInterpolationWizard( this, relaxationFactor );
    final WizardDialog wizardDialog = new WizardDialog( shell, wizard );
    if( wizardDialog.open() != Window.OK )
      return null;
    // select the new row; in ui job, as table is also updated in an ui event
    new UIJob( "" ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        viewer.setSelection( new StructuredSelection( m_resultRecords.toArray( new IRecord[] {} ) ) );
        return Status.OK_STATUS;
      }
    }.schedule();
    return null;
  }

  public void doInterpolate( final INTERPOLATION_METHOD interpolationMethod, final int numberOfSteps, final long timeInterval, final String relaxationFactor ) throws IndexOutOfBoundsException
  {
    m_resultRecords.clear();

    final Object firstInSelection = m_selection.getFirstElement();
    Object lastInSelection = firstInSelection;
    final Iterator< ? > iterator = m_selection.iterator();
    while( iterator.hasNext() )
      lastInSelection = iterator.next();
    final int firstIndex = m_tupleResult.indexOf( firstInSelection );
    final int lastIndex = m_tupleResult.indexOf( lastInSelection );
    final IRecord firstRecord = m_tupleResult.get( firstIndex );
    final IRecord lastRecord = m_tupleResult.get( lastIndex );
    final long timeMillisStart = ((XMLGregorianCalendar) firstRecord.getValue( 1 )).toGregorianCalendar().getTimeInMillis();
    final long timeMillisEnd = ((XMLGregorianCalendar) lastRecord.getValue( 1 )).toGregorianCalendar().getTimeInMillis();
    final TimeZone timeZone = ((XMLGregorianCalendar) firstRecord.getValue( 1 )).toGregorianCalendar().getTimeZone();

    final long timeStepMillis;
    switch( interpolationMethod )
    {
      case NUMBER_OF_STEPS:
        timeStepMillis = (timeMillisEnd - timeMillisStart) / (numberOfSteps - 1);
        break;

      case TIME_INTERVAL:
      default:
        timeStepMillis = timeInterval;
        break;
    }

    for( long currentTimeMillis = timeMillisStart; currentTimeMillis <= timeMillisEnd; currentTimeMillis += timeStepMillis )
    {
      final GregorianCalendar calendar = new GregorianCalendar( timeZone );
      calendar.setTimeInMillis( currentTimeMillis );
      final IRecord record = m_tupleResult.createRecord();
      record.setValue( 0, new BigInteger( "0" ) ); //$NON-NLS-1$
      record.setValue( 1, DateUtilities.toXMLGregorianCalendar( calendar ) );
      // changed to string to allow more flexible expansion of "Relaxation Factor"
      record.setValue( 2, new String( relaxationFactor ) );
      m_resultRecords.add( record );
    }

    // because selection doesn't have to be continuous...
    final List<IRecord> recordsToRemove = new ArrayList<>();
    for( int i = firstIndex; i <= lastIndex; i++ )
      recordsToRemove.add( m_tupleResult.get( i ) );

    m_tupleResult.removeAll( recordsToRemove );
    m_tupleResult.addAll( firstIndex, m_resultRecords );
  }
}