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
package org.kalypso.kalypsomodel1d2d.sim;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.sim.IterationInfo.IterationBean;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider;
import org.kalypso.ogc.gml.om.table.TupleResultLabelProvider;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoUIExtensions;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Show the iteration during calculation.
 * 
 * @author Gernot Belger
 */
public class IterationComposite extends Composite
{
  private static final String STR_NO_RESULTS = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationComposite.0" ); //$NON-NLS-1$

  private final DefaultTableViewer m_tableViewer;

  private final ComboViewer m_comboViewer;

  private final List<Object> m_comboInput = new ArrayList<Object>();

  private Object m_lastSelection;

  private final StatusComposite m_statusComposite;

  private final ICalculationUnit m_subUnit;

  public IterationComposite( final Composite composite, final IKalypsoSimulationRunnerComposite calculation, final ICalculationUnit subUnit, final int style )
  {
    super( composite, style );

    m_subUnit = subUnit;

    final Label unitLable = new Label( this, SWT.CENTER );
    unitLable.setText( m_subUnit.getName() + " - " + calculation.getCalculationTypeName() ); //$NON-NLS-1$
    unitLable.setFont( JFaceResources.getBannerFont() );

    m_tableViewer = new DefaultTableViewer( this, SWT.BORDER | SWT.FULL_SELECTION );
    final IComponentUiHandlerProvider provider = KalypsoUIExtensions.createComponentUiHandlerProvider( null );
    final TupleResultContentProvider cp = new TupleResultContentProvider( provider );
    m_tableViewer.setContentProvider( cp );
    m_tableViewer.setLabelProvider( new TupleResultLabelProvider( cp ) );

    final Table table = m_tableViewer.getTable();
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    table.setHeaderVisible( true );
    table.setVisible( false );

    /* Create control */
    setLayout( new GridLayout( 2, false ) );

    final Label comboLabel = new Label( this, SWT.NONE );
    comboLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationComposite.1" ) ); //$NON-NLS-1$

    m_comboViewer = new ComboViewer( this, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.LEFT, true, false ) );
    m_comboViewer.setContentProvider( new ArrayContentProvider() );

    m_comboViewer.setInput( m_comboInput );
    m_comboInput.add( STR_NO_RESULTS );
    m_comboViewer.refresh();
    m_comboViewer.setSelection( new StructuredSelection( STR_NO_RESULTS ) );
    m_comboViewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @SuppressWarnings("unchecked")
      @Override
      public String getText( final Object element )
      {
        if( element instanceof IterationBean )
          return ((IterationBean) element).name;

        if( element instanceof IObservation )
          return ((IObservation<TupleResult>) element).getName();

        return super.getText( element );
      }
    } );

    m_comboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleComboSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    m_statusComposite = new StatusComposite( this, StatusComposite.DETAILS );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    setStatus( null );

    m_comboViewer.getControl().setEnabled( false );

    final Job refreshJob = new UIJob( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationComposite.2" ) ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        try
        {
          updateControl( calculation );

          ProgressUtilities.done( monitor );
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }

        /* Periodically reschedule this task, if it was not canceled */
        if( calculation.getSimulationStatus() == null )
          schedule( 500 );
        else
          updateControl( calculation ); // update one last time, to make sure, current iter is removed from view

        return Status.OK_STATUS;
      }
    };
    refreshJob.setPriority( Job.INTERACTIVE );
    refreshJob.setUser( false );
    refreshJob.setSystem( true );
    refreshJob.schedule();

    addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        refreshJob.cancel();
      }
    } );
  }

  @SuppressWarnings("unchecked")
  protected void handleComboSelectionChanged( final IStructuredSelection selection )
  {
    setStatus( null );

    final Object selectedObject = selection.getFirstElement();

    // check if selection really changed, else do nothing
    if( m_lastSelection == selectedObject )
      return;

    if( selectedObject instanceof IObservation )
    {
      setTableInput( ((IObservation<TupleResult>) selectedObject).getResult() );
    }
    else if( selectedObject instanceof IterationBean )
    {
      final Cursor currentCursor = getCursor();
      try
      {
        final IterationBean bean = (IterationBean) selectedObject;
        if( !bean.status.isOK() )
          setStatus( bean.status );
        else
        {
          setCursor( getDisplay().getSystemCursor( SWT.CURSOR_WAIT ) );

          // read obs from file
          final GMLWorkspace obsWorkspace = GmlSerializer.createGMLWorkspace( bean.file.toURI().toURL(), null );
          final Feature obsFeature = obsWorkspace.getRootFeature();
          final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
          setTableInput( obs.getResult() );
        }
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationComposite.3" ), e ); //$NON-NLS-1$
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        setStatus( status );
      }
      finally
      {
        setCursor( currentCursor );
      }
    }

    m_lastSelection = selectedObject;
  }

  private void setStatus( final IStatus status )
  {
    m_statusComposite.setStatus( status );
    ((GridData) m_statusComposite.getLayoutData()).exclude = status == null;
    m_statusComposite.setVisible( status != null );

    m_tableViewer.getTable().setVisible( status == null );
    ((GridData) m_tableViewer.getTable().getLayoutData()).exclude = status != null;

    layout();
  }

  protected void updateControl( final IKalypsoSimulationRunnerComposite calculation )
  {
    final IIterationInfo iterationInfo = calculation.getIterationInfo();
    if( iterationInfo == null )
    {
      /* Calculation was not yet started */
      setTableInput( null );
      return;
    }

    final IObservation<TupleResult> iterObs = iterationInfo.getCurrentIteration();

    final Object[] iterations = iterationInfo.getIterations();

    final List<Object> comboIterations = new ArrayList<Object>();
    comboIterations.addAll( Arrays.asList( iterations ) );

    if( iterObs != null )
      comboIterations.add( iterObs );

    final Object oldComboSelection = ((IStructuredSelection) m_comboViewer.getSelection()).getFirstElement();

    if( m_comboViewer.getContentProvider() != null && comboIterations.size() > 0 )
    {
      m_comboInput.clear();
      m_comboInput.addAll( comboIterations );
      m_comboViewer.refresh();
      m_comboViewer.getControl().setEnabled( true );
    }

    if( oldComboSelection == null || oldComboSelection == STR_NO_RESULTS || oldComboSelection instanceof IObservation )
    {
      if( comboIterations.size() > 0 )
      {
        final Object lastElement = comboIterations.get( comboIterations.size() - 1 );
        m_comboViewer.setSelection( new StructuredSelection( lastElement ) );
      }
    }
  }

  private void setTableInput( final TupleResult tr )
  {
    final Object oldInput = m_tableViewer.getInput();
    if( oldInput == tr )
      return;

    // Set new input and refresh columns
    if( m_tableViewer.getContentProvider() != null )
      m_tableViewer.setInput( null );

    if( tr == null )
      return;

    m_tableViewer.setInput( tr );

    try
    {
      // set explicit the property of widget, in case of to few columns, setting of only first visible column is not
      // enough for right layout
      m_tableViewer.getTable().getColumn( 1 ).setData( "columnWidthPercent", 6 ); //$NON-NLS-1$
      m_tableViewer.getTable().getColumn( 1 ).setData( "columnWidth", 24 ); //$NON-NLS-1$
    }
    catch( Exception e )
    {
    }
    m_tableViewer.getTable().getColumn( 1 ).setWidth( 24 );

    TableColumn[] columns = m_tableViewer.getTable().getColumns();
    int lColumnsCount = columns.length;
    for( int i = 2; i < lColumnsCount; ++i )
    {
      final TableColumn lColumnIter = columns[i];
      if( lColumnIter != null )
      {
        try
        {
          lColumnIter.setData( "columnWidthPercent", 90 / lColumnsCount - 2 ); //$NON-NLS-1$
        }
        catch( Exception e )
        {
        }
      }
    }

    /* Always reveal the last element */
    final Object[] records = tr.toArray(); // use to array, the tuple result may be changing
    if( records.length > 0 )
      m_tableViewer.setSelection( new StructuredSelection( records[records.length - 1] ), true );
  }

}
