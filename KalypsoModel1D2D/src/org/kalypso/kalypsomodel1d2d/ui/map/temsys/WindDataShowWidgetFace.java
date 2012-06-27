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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;

/**
 * 
 * @author ig
 */
class WindDataShowWidgetFace
{
  private Composite rootPanel;

  private FormToolkit toolkit;

  private Section windSelectSection;

  private boolean isAnimating = false;

  final WindDataWidgetDataModel m_dataModel;

  WindDataModelSystemEditorComponent eleSystemEditorComponent;

  public WindDataShowWidgetFace( final WindDataWidgetDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public Control createControl( final Composite parent )
  {
    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    toolkit = new FormToolkit( parent.getDisplay() );
    final ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // Creates Section for "Select wind Model"
    windSelectSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    windSelectSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataShowWidgetFace.5" ) ); //$NON-NLS-1$ 
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    windSelectSection.setLayoutData( tableWrapData );
    windSelectSection.setExpanded( true );

    final Button lButton = toolkit.createButton( scrolledForm.getBody(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataShowWidgetFace.6" ), SWT.PUSH ); //$NON-NLS-1$  
    lButton.setSize( 30, 15 );
    lButton.addSelectionListener( new SelectionAdapter()
    {
      private AnimationOperation m_AnimationThread = null;

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Button lParentButton = (Button) e.getSource();
        if( m_AnimationThread != null && m_AnimationThread.isRunnig() )
        {
          m_AnimationThread.setRunnig( false );
          setAnimating( false );
          return;
        }
        setAnimating( true );

        lParentButton.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataShowWidgetFace.7" ) ); //$NON-NLS-1$ 

        m_AnimationThread = new AnimationOperation( lParentButton.getDisplay(), new NullProgressMonitor(), lParentButton );
        m_AnimationThread.setRunnig( true );
        m_AnimationThread.start();

      }
    } );

    createSelectWindDataModel( windSelectSection );
    return rootPanel;
  }

  private final void createSelectWindDataModel( final Section workStatusSection )
  {
    workStatusSection.setLayout( new FillLayout() );

    final Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    final FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );
    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 5 );
    clientComposite.setLayoutData( formData );
    eleSystemEditorComponent = new WindDataModelSystemEditorComponent();
    eleSystemEditorComponent.createControl( m_dataModel, clientComposite );
  }

  public void disposeControl( )
  {
    if( rootPanel == null )
    {
      return;
    }
    if( !rootPanel.isDisposed() )
    {
      rootPanel.dispose();
      toolkit.dispose();
    }

  }

  public final boolean isAnimating( )
  {
    return isAnimating;
  }

  public final void setAnimating( final boolean pAnimating )
  {
    this.isAnimating = pAnimating;
  }

  /**
   * @author ig
   * 
   * This class is responsible for triggering of repaint of the the wind theme, this done in thread this given
   * or predefined timeout. Name of the trigger-button is also changed according to the actual state.
   */
  private class AnimationOperation extends Thread
  {

    final IProgressMonitor m_progressBar;

    int m_intDone = 0;

    int m_intPauseTime = 500;

    private boolean m_isRunnig = false;

    private final Display m_display;

    final Button m_button;

    public AnimationOperation( final Display pDisplay, final IProgressMonitor pProgressBar, final Button parentButton )
    {
      this.m_display = pDisplay;
      this.m_progressBar = pProgressBar;
      this.m_button = parentButton;
    }

    @Override
    public void run( )
    {
      m_progressBar.beginTask( "", 100 ); //$NON-NLS-1$ 

      final IWindDataModelSystem lWindDataModelSystem = m_dataModel.getWindDataModelSystem();
      for( final IWindDataModel lWindModel : lWindDataModelSystem.getWindDataModels() )
      {
        try
        {
          Thread.sleep( m_intPauseTime );
        }
        catch( final InterruptedException e )
        {
        }
        m_display.asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            m_progressBar.worked( m_intDone++ );
            eleSystemEditorComponent.refreshActualWindView( lWindDataModelSystem, lWindModel );
          }
        } );
        if( !isRunnig() )
        {
          break;
        }
      }

      finishOperation();
    }

    private void finishOperation( )
    {
      m_display.asyncExec( new Runnable()
      {
        @Override
        public void run( )
        {
          m_progressBar.done();
          eleSystemEditorComponent.handleSelectionChanged( null );
          m_button.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataShowWidgetFace.6" ) ); //$NON-NLS-1$ 
          setAnimating( false );
        }
      } );
      setRunnig( false );
    }

    public final boolean isRunnig( )
    {
      return m_isRunnig;
    }

    public final void setRunnig( final boolean pRunnig )
    {
      this.m_isRunnig = pRunnig;
    }

    @SuppressWarnings("unused")
    public final void setIntPauseTime( final int intPauseTime )
    {
      this.m_intPauseTime = intPauseTime;
    }

  }

}