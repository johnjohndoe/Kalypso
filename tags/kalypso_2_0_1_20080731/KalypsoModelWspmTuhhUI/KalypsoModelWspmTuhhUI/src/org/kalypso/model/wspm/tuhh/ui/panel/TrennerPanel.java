/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
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
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.operation.changes.VisibleMarkerEdit;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class TrennerPanel extends AbstractProfilView
{

  protected Text m_fzl_text;

  protected Text m_fzr_text;

  protected Text m_dbl_text;

  protected Text m_dbr_text;

  protected Text m_bvl_text;

  protected Text m_bvr_text;

  protected Combo m_fzl_combo;

  protected Combo m_fzr_combo;

  protected Button m_fz_show;

  protected Button m_db_show;

  protected Button m_bv_show;

  protected Button m_db_add;

  protected Button m_db_del;

  protected Button m_bv_add;

  public TrennerPanel( final IProfil profile, final ProfilViewData viewdata )
  {
    super( profile, viewdata );
  }

  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final IProfil profil = getProfil();
    final Display display = parent.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );
    final Composite panel = new Composite( parent, style );
    panel.setLayout( new GridLayout( 1, true ) );

    final Group fliesszoneGroup = new Group( panel, SWT.NONE );
    fliesszoneGroup.setText( "Trennflächen" );
    fliesszoneGroup.setLayout( new GridLayout( 2, false ) );
    fliesszoneGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    final Label posllabel = new Label( fliesszoneGroup, SWT.NONE );
    posllabel.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_CENTER ) );
    posllabel.setText( "links" );
    final Label posrlabel = new Label( fliesszoneGroup, SWT.NONE );
    posrlabel.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_CENTER ) );
    posrlabel.setText( "rechts" );
    m_fzl_text = new Text( fliesszoneGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_fzl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fzl_text.addModifyListener( doubleModifyListener );
    m_fzl_text.addFocusListener( new TrennerFocusListener( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ), 0 ) );

    m_fzr_text = new Text( fliesszoneGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_fzr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fzr_text.addModifyListener( doubleModifyListener );
    m_fzr_text.addFocusListener( new TrennerFocusListener( profil.hasPointProperty(IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ), 1 ) );

    m_fzl_combo = new Combo( fliesszoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_fzl_combo.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );

    m_fzl_combo.add( "Böschungsfuß" );
    m_fzl_combo.add( "Vorland" );
    m_fzl_combo.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) ;
        if( deviders.length < 1 )
          return;
        final IProfilPointMarker devider = deviders[0];
        final PointMarkerEdit edit = new PointMarkerEdit( devider, m_fzl_combo.getSelectionIndex() == 0 );

        final ProfilOperation operation = new ProfilOperation( "Lage der Trennfläche ändern", profil, edit, true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    m_fzr_combo = new Combo( fliesszoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_fzr_combo.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );

    m_fzr_combo.add( "Böschungsfuß" );
    m_fzr_combo.add( "Vorland" );
    m_fzr_combo.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IProfilPointMarker[] deviders = profil.getPointMarkerFor(IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) ;
        if( deviders.length < 2 )
          return;
        final IProfilPointMarker devider = deviders[1];

        final PointMarkerEdit edit = new PointMarkerEdit( devider, m_fzr_combo.getSelectionIndex() == 0 );

        final ProfilOperation operation = new ProfilOperation( "Lage der Trennfläche ändern", profil, edit, true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    m_fz_show = new Button( fliesszoneGroup, SWT.CHECK );
    final GridData fz_showData = new GridData();
    fz_showData.horizontalSpan = 2;
    m_fz_show.setLayoutData( fz_showData );
    m_fz_show.setText( "Trennflächen anzeigen" );
    m_fz_show.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final ProfilOperation operation = new ProfilOperation( "",profil, true );
        operation.addChange( new VisibleMarkerEdit( getViewData(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, m_fz_show.getSelection() ) );
        final IStatus status = operation.execute( new NullProgressMonitor(), null );
        operation.dispose();
        if( !status.isOK() )
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      }
    } );

    final Group durchstroemteGroup = new Group( panel, SWT.NONE );
    durchstroemteGroup.setText( "Durchströmter Bereich" );
    durchstroemteGroup.setLayout( new GridLayout( 2, false ) );
    durchstroemteGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_dbl_text = new Text( durchstroemteGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbl_text.addModifyListener( doubleModifyListener );
    m_dbl_text.addFocusListener( new TrennerFocusListener(profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ), 0 ) );

    m_dbr_text = new Text( durchstroemteGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbr_text.addModifyListener( doubleModifyListener );
    m_dbr_text.addFocusListener( new TrennerFocusListener( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ), 1 ) );

    m_db_show = new Button( durchstroemteGroup, SWT.CHECK );
    final GridData db_showData = new GridData();
    db_showData.horizontalSpan = 2;
    m_db_show.setLayoutData( db_showData );
    m_db_show.setText( "Durchströmten Bereich anzeigen" );
    m_db_show.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final ProfilOperation operation = new ProfilOperation( "", profil, true );
        operation.addChange( new VisibleMarkerEdit( getViewData(), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, m_db_show.getSelection() ) );
        final IStatus status = operation.execute( new NullProgressMonitor(), null );
        operation.dispose();
        if( !status.isOK() )
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );

      }
    } );

    final Group bordvollGroup = new Group( panel, SWT.NONE );
    bordvollGroup.setText( "Bordvollpunkte" );
    bordvollGroup.setLayout( new GridLayout( 2, false ) );
    bordvollGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_bvl_text = new Text( bordvollGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvl_text.addModifyListener( doubleModifyListener );
    m_bvl_text.addFocusListener( new TrennerFocusListener( profil.hasPointProperty(IWspmTuhhConstants.MARKER_TYP_BORDVOLL ), 0 ) );

    m_bvr_text = new Text( bordvollGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvr_text.addModifyListener( doubleModifyListener );
    m_bvr_text.addFocusListener( new TrennerFocusListener(profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ), 1 ) );

    m_bv_show = new Button( bordvollGroup, SWT.CHECK );
    final GridData bv_showData = new GridData();
    bv_showData.horizontalSpan = 2;
    m_bv_show.setLayoutData( bv_showData );
    m_bv_show.setText( "Bordvollpunkte anzeigen" );
    m_bv_show.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final ProfilOperation operation = new ProfilOperation( "Sichtbarkeit ändern:",profil, true );
        operation.addChange( new VisibleMarkerEdit( getViewData(), IWspmTuhhConstants.MARKER_TYP_BORDVOLL, m_bv_show.getSelection() ) );
        final IStatus status = operation.execute( new NullProgressMonitor(), null );
        operation.dispose();
        if( !status.isOK() )
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      }
    } );

    m_bv_add = new Button( bordvollGroup, SWT.NONE );
    final GridData bv_addData = new GridData( GridData.FILL_HORIZONTAL );
    bv_addData.horizontalSpan = 2;
    m_bv_add.setLayoutData( bv_addData );
    m_bv_add.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final IProfilPointMarker[] bv_devs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) ;

        if( bv_devs.length < 1 )
        {

          final IProfilPointMarker[] db_devs = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
          if( db_devs.length < 2 )
            return;
          final ProfilOperation operation = new ProfilOperation( "Bordvollpunkte einfügen:", profil, true );

          final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );
          final IComponent bordvoll = provider.getPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );

          if( !profil.hasPointProperty( bordvoll ) )
            profil.addPointProperty( bordvoll );

          operation.addChange( new PointMarkerEdit( new ProfilDevider( bordvoll, db_devs[0].getPoint() ), true ) );
          operation.addChange( new PointMarkerEdit( new ProfilDevider( bordvoll, db_devs[1].getPoint() ), true ) );
          operation.addChange( new VisibleMarkerEdit( getViewData(), bordvoll.getId(), true ) );

          operation.addChange( new ActiveObjectEdit( profil, db_devs[1].getPoint(), null ) );
          new ProfilOperationJob( operation ).schedule();

        }
        else
        {
// final IProfilChange[] changes = new IProfilChange[bv_devs.length + 1];
// for( int i = 0; i < bv_devs.length; i++ )
// {
// changes[i] = new PointMarkerEdit( bv_devs[i], null );
// }
// changes[bv_devs.length] = new VisibleMarkerEdit( getViewData(), IWspmTuhhConstants.MARKER_TYP_BORDVOLL, false );

          // final ProfilOperation operation = new ProfilOperation( "Bordvollpunkte entfernen:", getProfil(), changes,
          // true );
          final ProfilOperation operation = new ProfilOperation( "Bordvollpunkte entfernen:",profil, true );
          operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) ) );
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );

    updateControls();

    return panel;
  }

  /**
   * @exception SWTException
   *                <ul>
   *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the thread that created the receiver</li>
   *                </ul>
   */
  protected void updateControls( )
  {final IProfil profil = getProfil();
    final IProfilPointMarker[] fz_devs = profil.getPointMarkerFor(IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE  );
    final IProfilPointMarker[] db_devs = profil.getPointMarkerFor(  IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE  );
    final IProfilPointMarker[] bv_devs = profil.getPointMarkerFor(  IWspmTuhhConstants.MARKER_TYP_BORDVOLL  );

    try
    {
      if( !m_fz_show.isDisposed() )
      {
        m_fz_show.setSelection( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
      }
      if( (!m_fzl_text.isDisposed()) && (fz_devs.length > 0) )
        m_fzl_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor(  IWspmTuhhConstants.POINT_PROPERTY_BREITE ,fz_devs[0].getPoint())));
      else
        m_fzl_text.setText( "unbekannt" );
      if( !m_fzr_text.isDisposed() && (fz_devs.length > 1) )
        m_fzr_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor(  IWspmTuhhConstants.POINT_PROPERTY_BREITE ,fz_devs[1].getPoint())));
      else
        m_fzr_text.setText( "unbekannt" );
      if( !m_fzl_combo.isDisposed() && (fz_devs.length > 0) )
      {
        final Boolean valueFor = (Boolean) fz_devs[0].getIntepretedValue();
        if( valueFor == null || valueFor )
        {
          m_fzl_combo.select( 0 );
        }
        else
        {
          m_fzl_combo.select( 1 );
        }
      }
      if( !m_fzr_combo.isDisposed() && (fz_devs.length > 1) )
      {
        final Boolean valueFor = (Boolean) fz_devs[1].getIntepretedValue();
        if( valueFor == null || valueFor )
        {
          m_fzr_combo.select( 0 );
        }
        else
        {
          m_fzr_combo.select( 1 );
        }
      }
      if( !m_db_show.isDisposed() )
      {
        m_db_show.setSelection( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
      }
      if( !m_dbl_text.isDisposed() && (db_devs.length > 0) )
        m_dbl_text.setText( String.format( "%.4f",ProfilUtil.getDoubleValueFor(  IWspmTuhhConstants.POINT_PROPERTY_BREITE ,db_devs[0].getPoint())));
      else
        m_dbl_text.setText( "unbekannt" );
      if( !m_dbr_text.isDisposed() && (db_devs.length > 1) )
        m_dbr_text.setText( String.format( "%.4f",ProfilUtil.getDoubleValueFor(  IWspmTuhhConstants.POINT_PROPERTY_BREITE ,db_devs[1].getPoint())));
      else
        m_dbr_text.setText( "unbekannt" );
      if( bv_devs.length == 0 )
      {
        if( !m_bvl_text.isDisposed() )
        {
          m_bvl_text.setVisible( false );
        }
        if( !m_bvr_text.isDisposed() )
        {
          m_bvr_text.setVisible( false );
        }
        if( !m_bv_show.isDisposed() )
        {
          m_bv_show.setVisible( false );
        }
        if( !m_bv_add.isDisposed() )
        {
          m_bv_add.setText( "Bordvollpunkte einfügen" );
        }
      }
      else
      {
        if( !m_bv_show.isDisposed() )
        {
          m_bv_show.setVisible( true );
          m_bv_show.setSelection( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
        }
        if( !m_bv_add.isDisposed() )
        {
          m_bv_add.setText( "Bordvollpunkte entfernen" );
        }
        if( !m_bvl_text.isDisposed() && (bv_devs.length > 0) )
        {
          m_bvl_text.setVisible( true );
          m_bvl_text.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor(  IWspmTuhhConstants.POINT_PROPERTY_BREITE ,bv_devs[0].getPoint())));

        }
        else
          m_bvl_text.setText( "unbekannt" );
        if( !m_bvr_text.isDisposed() && (bv_devs.length > 1) )
        {
          m_bvr_text.setVisible( true );
          m_bvr_text.setText( String.format( "%.4f",ProfilUtil.getDoubleValueFor(  IWspmTuhhConstants.POINT_PROPERTY_BREITE ,bv_devs[1].getPoint())));
        }
        else
          m_bvr_text.setText( "unbekannt" );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isProfilPropertyChanged() || hint.isPointsChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() )
    {

      final Control control = getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
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
    private final IComponent m_type;

    private final int m_pos;

    public TrennerFocusListener( final IComponent type, final int pos )
    {
      m_type = type;
      m_pos = pos;
    }

    /**
     * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
     */
    public void focusGained( final FocusEvent e )
    {
      if( e.widget instanceof Text )
        ((Text) e.widget).selectAll();
    }

    /**
     * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
     */
    public void focusLost( final FocusEvent e )
    {
      if( e.widget instanceof Text )
      {
        final Text text = (Text) e.widget;
        final IProfil profil = getProfil();
        final double value = NumberUtils.parseQuietDouble( text.getText() );
        final IProfilPointMarker[] devs = profil.getPointMarkerFor( m_type );

        if( Double.isNaN( value ) )
        {
          updateControls();
          return;
        }

        final IRecord pointCloseTo = ProfilUtil.findNearestPoint(profil, value );
        final ProfilOperation operation = new ProfilOperation( "",profil, true );

        if( devs.length <= m_pos )
        {
          operation.setLabel( m_type.toString() + " hinzufügen" );
          operation.addChange( new PointMarkerEdit( new ProfilDevider( m_type, pointCloseTo ), true ) );
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

          operation.setLabel( key.toString() + " verschieben" );
          operation.addChange( new PointMarkerSetPoint( key, pointCloseTo ) );
          operation.addChange( new ActiveObjectEdit( profil, pointCloseTo, null ) );
        }

        new ProfilOperationJob( operation ).schedule();
      }
    }
  }
}