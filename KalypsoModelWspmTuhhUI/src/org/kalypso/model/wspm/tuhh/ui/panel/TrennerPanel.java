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
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class TrennerPanel extends AbstractProfilView
{
  private Text m_fzl_text;

  private Text m_fzr_text;

  private Text m_dbl_text;

  private Text m_dbr_text;

  private Text m_bvl_text;

  private Text m_bvr_text;

  private ComboViewer m_fzl_combo;

  private ComboViewer m_fzr_combo;

  Button m_bv_add;

  private Composite m_panel;

  public TrennerPanel( final IProfil profile )
  {
    super( profile );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit)
  {
    final IProfil profil = getProfil();
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

    m_fzl_text = toolkit.createText( fliesszoneGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_fzl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fzl_text.addModifyListener( doubleModifyListener );
    m_fzl_text.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, 0 ) );

    m_fzr_text = toolkit.createText( fliesszoneGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_fzr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fzr_text.addModifyListener( doubleModifyListener );
    m_fzr_text.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, 1 ) );

    m_fzl_combo = new ComboViewer( fliesszoneGroup, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY );
    m_fzl_combo.getCombo().setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_fzl_combo.setContentProvider( new ArrayContentProvider() );
    m_fzl_combo.setInput( new Boolean[] { true, false } );

    m_fzl_combo.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element.equals( true ) )
          return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.3" ); //$NON-NLS-1$
        return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.4" ); //$NON-NLS-1$
      }
    } );

    m_fzl_combo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        final Boolean value = (Boolean) selection.getFirstElement();

        final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
        if( deviders.length < 1 )
          return;
        final IProfilPointMarker devider = deviders[0];
        if( devider.getIntepretedValue() == value )
          return;
        final PointMarkerEdit edit = new PointMarkerEdit( devider, value );

        final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.5" ), profil, edit, true ); //$NON-NLS-1$
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    m_fzr_combo = new ComboViewer( fliesszoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_fzr_combo.getCombo().setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_fzr_combo.setContentProvider( new ArrayContentProvider() );
    m_fzr_combo.setInput( new Boolean[] { true, false } );

    m_fzr_combo.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element.equals( true ) )
          return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.3" ); //$NON-NLS-1$
        return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.4" ); //$NON-NLS-1$
      }
    } );

    m_fzr_combo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        final Boolean value = (Boolean) selection.getFirstElement();

        final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
        if( deviders.length < 2 )
          return;
        final IProfilPointMarker devider = deviders[1];
        if( devider.getIntepretedValue() == value )
          return;
        final PointMarkerEdit edit = new PointMarkerEdit( devider, value );

        final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.8" ), profil, edit, true ); //$NON-NLS-1$
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    toolkit.adapt( m_fzr_combo.getCombo() );
    toolkit.adapt( m_fzl_combo.getCombo() );

    final Group durchstroemteGroup = new Group( m_panel, SWT.NONE );
    durchstroemteGroup.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.9" ) ); //$NON-NLS-1$
    durchstroemteGroup.setLayout( new GridLayout( 2, true ) );
    durchstroemteGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    toolkit.adapt( durchstroemteGroup );

    m_dbl_text = toolkit.createText( durchstroemteGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbl_text.addModifyListener( doubleModifyListener );
    m_dbl_text.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, 0 ) );

    m_dbr_text = toolkit.createText( durchstroemteGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbr_text.addModifyListener( doubleModifyListener );
    m_dbr_text.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, 1 ) );

    final Group bordvollGroup = new Group( m_panel, SWT.NONE );
    bordvollGroup.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.10" ) ); //$NON-NLS-1$
    bordvollGroup.setLayout( new GridLayout( 2, true ) );
    bordvollGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    toolkit.adapt( bordvollGroup );

    m_bv_add = toolkit.createButton( bordvollGroup, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.11" ), SWT.CHECK ); //$NON-NLS-1$
    m_bv_add.setSelection( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) != null );
    final GridData bv_addData = new GridData( GridData.FILL_HORIZONTAL );
    bv_addData.horizontalSpan = 2;
    m_bv_add.setLayoutData( bv_addData );
    m_bv_add.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final boolean selected = m_bv_add.getSelection();
        if( selected )
        {
          final IProfilPointMarker[] db_devs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

          final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.12" ), profil, true ); //$NON-NLS-1$

          final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );
          final IComponent bordvoll = provider.getPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );

          operation.addChange( new PointPropertyAdd( profil, bordvoll ) );

          final IRecord[] profilPoints = profil.getPoints();
          if( profilPoints.length > 0 )
          {
            final IRecord firstPoint = profilPoints[0];
            final IRecord lastPoint = profilPoints[profilPoints.length - 1];

            final IRecord leftPoint = db_devs.length > 0 ? db_devs[0].getPoint() : firstPoint;
            final IRecord rightPoint = db_devs.length > 1 ? db_devs[1].getPoint() : lastPoint;
            operation.addChange( new PointMarkerEdit( new ProfilDevider( bordvoll, leftPoint ), true ) );
            operation.addChange( new PointMarkerEdit( new ProfilDevider( bordvoll, rightPoint ), true ) );
            operation.addChange( new ActiveObjectEdit( profil, rightPoint, bordvoll ) );
          }

          new ProfilOperationJob( operation ).schedule();
        }
        else
        {
          final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.13" ), profil, true ); //$NON-NLS-1$
          operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) ) );
          new ProfilOperationJob( operation ).schedule();
        }

      }
    } );

    m_bvl_text = toolkit.createText( bordvollGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvl_text.addModifyListener( doubleModifyListener );
    m_bvl_text.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, 0 ) );

    m_bvr_text = toolkit.createText( bordvollGroup, null, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvr_text.addModifyListener( doubleModifyListener );
    m_bvr_text.addFocusListener( new TrennerFocusListener( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, 1 ) );

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

    final IProfil profil = getProfil();
    final IProfilPointMarker[] tf_devs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker[] db_devs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final IProfilPointMarker[] bv_devs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );

    final boolean hasBV = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) != null;

    if( tf_devs.length > 0 )
    {
      m_fzl_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, tf_devs[0].getPoint() ) ) ); //$NON-NLS-1$
      final Object intepretedValue = tf_devs[0].getIntepretedValue();
      if( intepretedValue == null )
        System.out.println( "null" ); //$NON-NLS-1$
      else
        m_fzl_combo.setSelection( new StructuredSelection( intepretedValue ) );
    }
    else
    {
      m_fzl_text.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
      m_fzl_combo.setSelection( new StructuredSelection() );
    }

    if( tf_devs.length > 1 )
    {
      m_fzr_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, tf_devs[1].getPoint() ) ) ); //$NON-NLS-1$
      final Object intepretedValue = tf_devs[1].getIntepretedValue();
      if( intepretedValue == null )
        System.out.println( "null" ); //$NON-NLS-1$
      else
        m_fzr_combo.setSelection( new StructuredSelection( intepretedValue ) );
    }
    else
    {
      m_fzr_text.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
      m_fzr_combo.setSelection( new StructuredSelection() );
    }

    if( db_devs.length > 0 )
      m_dbl_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, db_devs[0].getPoint() ) ) ); //$NON-NLS-1$
    else
      m_dbl_text.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$

    if( db_devs.length > 1 )
      m_dbr_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, db_devs[1].getPoint() ) ) ); //$NON-NLS-1$
    else
      m_dbr_text.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$

    if( bv_devs.length > 0 )
    {
      m_bvl_text.setVisible( true );
      m_bvr_text.setVisible( true );
      m_bvl_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, bv_devs[0].getPoint() ) ) ); //$NON-NLS-1$
    }
    else
    {
      m_bvl_text.setVisible( false );
      m_bvr_text.setVisible( false );
      m_bvl_text.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
    }

    if( bv_devs.length > 1 )
    {
      m_bvr_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, bv_devs[1].getPoint() ) ) ); //$NON-NLS-1$
    }
    else
    {
      m_bvr_text.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.15" ) ); //$NON-NLS-1$
    }

    m_bv_add.setSelection( hasBV );
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isPointPropertiesChanged() || hint.isPointValuesChanged()||hint.isPointsChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() || hint.isProfilPropertyChanged() )
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
        ((Text) e.widget).selectAll();
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
        final IProfil profil = getProfil();
        final double value = NumberUtils.parseQuietDouble( text.getText() );
        final IProfilPointMarker[] devs = profil.getPointMarkerFor( m_component );
        final IComponent type = profil.hasPointProperty( m_component );
        if( type == null )
          return;

        if( Double.isNaN( value ) )
        {
          updateControls();
          return;
        }

        final IRecord pointCloseTo = ProfilUtil.findNearestPoint( profil, value );
        final ProfilOperation operation = new ProfilOperation( "", profil, true ); //$NON-NLS-1$

        if( devs.length <= m_pos )
        {
          operation.setLabel( type.toString() + Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.27" ) ); //$NON-NLS-1$
          operation.addChange( new PointMarkerEdit( new ProfilDevider( type, pointCloseTo ), true ) );
        }
        else
        {
          final IProfilPointMarker key = devs[m_pos];

          final IRecord oldPos = key.getPoint();
          if( oldPos == pointCloseTo )
          {
            updateControls();
            return;
          }

          operation.setLabel( key.toString() + Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel.28" ) ); //$NON-NLS-1$
          operation.addChange( new PointMarkerSetPoint( key, pointCloseTo ) );
          operation.addChange( new ActiveObjectEdit( profil, pointCloseTo, null ) );
        }

        new ProfilOperationJob( operation ).schedule();
      }
    }
  }
}