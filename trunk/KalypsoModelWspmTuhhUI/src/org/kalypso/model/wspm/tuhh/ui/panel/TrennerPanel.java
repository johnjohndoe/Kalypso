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
package org.kalypso.model.wspm.tuhh.ui.panel;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class TrennerPanel extends AbstractProfilView
{
  private Text m_fzl;

  private Text m_fzr;

  private Text m_dbl;

  private Text m_dbr;

  private Text m_bvl;

  private Text m_bvr;

  private ComboViewer m_cFzl;

  private ComboViewer m_cFzr;

  Button m_bAdd;

  private Composite m_panel;

  public TrennerPanel( final IProfile profile )
  {
    super( profile );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    final IProfile profil = getProfile();
    final Display display = parent.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );
    m_panel = toolkit.createComposite( parent, SWT.NONE );
    m_panel.setLayout( new GridLayout( 1, true ) );

    final Group fliesszoneGroup = new Group( m_panel, SWT.NONE );
    fliesszoneGroup.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.0" ) ); //$NON-NLS-1$
    fliesszoneGroup.setLayout( new GridLayout( 2, true ) );
    fliesszoneGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    toolkit.adapt( fliesszoneGroup );

    m_fzl = toolkit.createText( fliesszoneGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_fzl.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fzl.addModifyListener( doubleModifyListener );
    m_fzl.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, 0 ) );

    m_fzr = toolkit.createText( fliesszoneGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_fzr.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fzr.addModifyListener( doubleModifyListener );
    m_fzr.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, 1 ) );

    m_cFzl = new ComboViewer( fliesszoneGroup, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY );
    m_cFzl.getCombo().setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_cFzl.setContentProvider( new ArrayContentProvider() );
    m_cFzl.setInput( new Boolean[] { true, false } );

    m_cFzl.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( Boolean.TRUE.equals( element ) )
          return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.3" ); //$NON-NLS-1$
        return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.4" ); //$NON-NLS-1$
      }
    } );

    m_cFzl.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        final Boolean value = (Boolean) selection.getFirstElement();

        final IProfilePointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
        if( deviders.length < 1 )
          return;
        final IProfilePointMarker devider = deviders[0];
        if( devider.getIntepretedValue() == value )
          return;
        final PointMarkerEdit edit = new PointMarkerEdit( devider, value );

        final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.5" ), profil, edit, true ); //$NON-NLS-1$
        new ProfileOperationJob( operation ).schedule();
      }
    } );

    m_cFzr = new ComboViewer( fliesszoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_cFzr.getCombo().setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_cFzr.setContentProvider( new ArrayContentProvider() );
    m_cFzr.setInput( new Boolean[] { true, false } );

    m_cFzr.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( Boolean.TRUE.equals( element ) )
          return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.3" ); //$NON-NLS-1$
        return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.4" ); //$NON-NLS-1$
      }
    } );

    m_cFzr.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        final Boolean value = (Boolean) selection.getFirstElement();

        final IProfilePointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
        if( deviders.length < 2 )
          return;
        final IProfilePointMarker devider = deviders[1];
        if( devider.getIntepretedValue() == value )
          return;
        final PointMarkerEdit edit = new PointMarkerEdit( devider, value );

        final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.8" ), profil, edit, true ); //$NON-NLS-1$
        new ProfileOperationJob( operation ).schedule();
      }
    } );

    toolkit.adapt( m_cFzr.getCombo() );
    toolkit.adapt( m_cFzl.getCombo() );

    final Group durchstroemteGroup = new Group( m_panel, SWT.NONE );
    durchstroemteGroup.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.9" ) ); //$NON-NLS-1$
    durchstroemteGroup.setLayout( new GridLayout( 2, true ) );
    durchstroemteGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    toolkit.adapt( durchstroemteGroup );

    m_dbl = toolkit.createText( durchstroemteGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbl.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbl.addModifyListener( doubleModifyListener );
    m_dbl.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, 0 ) );

    m_dbr = toolkit.createText( durchstroemteGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbr.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbr.addModifyListener( doubleModifyListener );
    m_dbr.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, 1 ) );

    final Group bordvollGroup = new Group( m_panel, SWT.NONE );
    bordvollGroup.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.10" ) ); //$NON-NLS-1$
    bordvollGroup.setLayout( new GridLayout( 2, true ) );
    bordvollGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    toolkit.adapt( bordvollGroup );

    m_bAdd = toolkit.createButton( bordvollGroup, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.11" ), SWT.CHECK ); //$NON-NLS-1$
    m_bAdd.setSelection( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) != null );
    final GridData bvAddData = new GridData( GridData.FILL_HORIZONTAL );
    bvAddData.horizontalSpan = 2;
    m_bAdd.setLayoutData( bvAddData );
    m_bAdd.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final boolean selected = m_bAdd.getSelection();
        if( selected )
        {
          final IProfilePointMarker[] dbDevs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

          final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.12" ), profil, true ); //$NON-NLS-1$

          final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );
          final IComponent bordvoll = provider.getPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );

          operation.addChange( new PointPropertyAdd( profil, bordvoll ) );

          final IProfileRecord[] profilPoints = profil.getPoints();
          if( profilPoints.length > 0 )
          {
            final IProfileRecord firstPoint = profilPoints[0];
            final IProfileRecord lastPoint = profilPoints[profilPoints.length - 1];

            final IProfileRecord leftPoint = dbDevs.length > 0 ? dbDevs[0].getPoint() : firstPoint;
            final IProfileRecord rightPoint = dbDevs.length > 1 ? dbDevs[1].getPoint() : lastPoint;
            operation.addChange( new PointMarkerEdit( new ProfilDevider( bordvoll, leftPoint ), true ) );
            operation.addChange( new PointMarkerEdit( new ProfilDevider( bordvoll, rightPoint ), true ) );
            operation.addChange( new ActiveObjectEdit( profil, rightPoint.getBreiteAsRange(), bordvoll ) );
          }

          new ProfileOperationJob( operation ).schedule();
        }
        else
        {
          final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.13" ), profil, true ); //$NON-NLS-1$
          operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) ) );
          new ProfileOperationJob( operation ).schedule();
        }

      }
    } );

    m_bvl = toolkit.createText( bordvollGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvl.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvl.addModifyListener( doubleModifyListener );
    m_bvl.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, 0 ) );

    m_bvr = toolkit.createText( bordvollGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvr.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvr.addModifyListener( doubleModifyListener );
    m_bvr.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, 1 ) );

    updateControls();

    return m_panel;
  }

  /**
   * @exception SWTException
   *              <ul>
   *              <li>ERROR_THREAD_INVALID_ACCESS - if not called from the thread that created the receiver</li>
   *              </ul>
   */
  protected void updateControls( )
  {
    if( m_panel == null || m_panel.isDisposed() )
      return;

    final IProfile profil = getProfile();
    final IProfilePointMarker[] tfDevs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilePointMarker[] dbDevs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final IProfilePointMarker[] bvDevs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );

    final boolean hasBV = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) != null;

    if( tfDevs.length > 0 )
    {
      m_fzl.setText( String.format( "%.4f", ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, tfDevs[0].getPoint() ) ) ); //$NON-NLS-1$
      final Object intepretedValue = tfDevs[0].getIntepretedValue();
      if( intepretedValue == null )
      {
        System.out.println( "null" ); //$NON-NLS-1$
      }
      else
      {
        m_cFzl.setSelection( new StructuredSelection( intepretedValue ) );
      }
    }
    else
    {
      m_fzl.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
      m_cFzl.setSelection( new StructuredSelection() );
    }

    if( tfDevs.length > 1 )
    {
      m_fzr.setText( String.format( "%.4f", ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, tfDevs[1].getPoint() ) ) ); //$NON-NLS-1$
      final Object intepretedValue = tfDevs[1].getIntepretedValue();
      if( intepretedValue == null )
      {
        System.out.println( "null" ); //$NON-NLS-1$
      }
      else
      {
        m_cFzr.setSelection( new StructuredSelection( intepretedValue ) );
      }
    }
    else
    {
      m_fzr.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
      m_cFzr.setSelection( new StructuredSelection() );
    }

    if( dbDevs.length > 0 )
    {
      m_dbl.setText( String.format( "%.4f", ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, dbDevs[0].getPoint() ) ) ); //$NON-NLS-1$
    }
    else
    {
      m_dbl.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
    }

    if( dbDevs.length > 1 )
    {
      m_dbr.setText( String.format( "%.4f", ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, dbDevs[1].getPoint() ) ) ); //$NON-NLS-1$
    }
    else
    {
      m_dbr.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
    }

    if( bvDevs.length > 0 )
    {
      m_bvl.setVisible( true );
      m_bvr.setVisible( true );
      m_bvl.setText( String.format( "%.4f", ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, bvDevs[0].getPoint() ) ) ); //$NON-NLS-1$
    }
    else
    {
      m_bvl.setVisible( false );
      m_bvr.setVisible( false );
      m_bvl.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
    }

    if( bvDevs.length > 1 )
    {
      m_bvr.setText( String.format( "%.4f", ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, bvDevs[1].getPoint() ) ) ); //$NON-NLS-1$
    }
    else
    {
      m_bvr.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
    }

    m_bAdd.setSelection( hasBV );
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isPointPropertiesChanged() || hint.isPointValuesChanged() || hint.isPointsChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() || hint.isProfilPropertyChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            updateControls();
          }
        } );
      }
    }
  }

  public class TrennerFocusListener implements FocusListener
  {
    private final String m_component;

    private final int m_pos;

    public TrennerFocusListener( final String component, final int pos )
    {
      m_component = component;
      m_pos = pos;
    }

    /**
     * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
     */
    @Override
    public void focusGained( final FocusEvent e )
    {
      if( e.widget instanceof Text )
      {
        ((Text) e.widget).selectAll();
      }
    }

    /**
     * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
     */
    @Override
    public void focusLost( final FocusEvent e )
    {
      if( e.widget instanceof Text )
      {
        final Text text = (Text) e.widget;
        final IProfile profil = getProfile();
        final double value = NumberUtils.parseQuietDouble( text.getText() );
        final IProfilePointMarker[] devs = profil.getPointMarkerFor( m_component );
        final IComponent type = profil.hasPointProperty( m_component );
        if( type == null )
          return;

        if( Double.isNaN( value ) )
        {
          updateControls();
          return;
        }

        final IProfileRecord pointCloseTo = ProfileVisitors.findNearestPoint( profil, value );
        final ProfileOperation operation = new ProfileOperation( "", profil, true ); //$NON-NLS-1$

        if( devs.length <= m_pos )
        {
          operation.setLabel( type.toString() + Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.27" ) ); //$NON-NLS-1$
          operation.addChange( new PointMarkerEdit( new ProfilDevider( type, pointCloseTo ), true ) );
        }
        else
        {
          final IProfilePointMarker key = devs[m_pos];

          final IRecord oldPos = key.getPoint();
          if( oldPos == pointCloseTo )
          {
            updateControls();
            return;
          }

          operation.setLabel( key.toString() + Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.28" ) ); //$NON-NLS-1$
          operation.addChange( new PointMarkerSetPoint( key, pointCloseTo ) );
          operation.addChange( new ActiveObjectEdit( profil, pointCloseTo.getBreiteAsRange(), null ) );
        }

        new ProfileOperationJob( operation ).schedule();
      }
    }
  }
}