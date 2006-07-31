package org.kalypso.model.wspm.ui.profil.view.panel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
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
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.DeviderAdd;
import org.kalypso.model.wspm.core.profil.changes.DeviderEdit;
import org.kalypso.model.wspm.core.profil.changes.DeviderMove;
import org.kalypso.model.wspm.core.profil.changes.DeviderRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.VisibleDeviderEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;

/**
 * @author gernot
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

  public TrennerPanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata, null );
  }

  @Override
  protected Control doCreateControl( final Composite parent, int style )
  {
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
    m_fzl_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {
        m_fzl_text.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final double value = NumberUtils.parseQuietDouble( m_fzl_text.getText() );
        final IProfilDevider[] devs = getProfil().getDevider( DEVIDER_TYP.TRENNFLAECHE );
        if( !Double.isNaN( value ) && devs != null && devs.length > 0 )
        {
          setValue( value, devs[0] );
        }
        else
          updateControls();
      }
    } );
    m_fzr_text = new Text( fliesszoneGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_fzr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fzr_text.addModifyListener( doubleModifyListener );
    m_fzr_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {
        m_fzr_text.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {

        final double value = NumberUtils.parseQuietDouble( m_fzr_text.getText() );
        final IProfilDevider[] devs = getProfil().getDevider( DEVIDER_TYP.TRENNFLAECHE );
        if( !Double.isNaN( value ) && devs != null && devs.length > 1 )
        {
          setValue( value, devs[1] );
        }
        else
          updateControls();
      }
    } );
    m_fzl_combo = new Combo( fliesszoneGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_fzl_combo.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );

    m_fzl_combo.add( "Böschungsfuß" );
    m_fzl_combo.add( "Vorland" );
    m_fzl_combo.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final IProfilDevider[] deviders = getProfil().getDevider( DEVIDER_TYP.TRENNFLAECHE );
        if( (deviders == null) || (deviders.length < 1) )
          return;
        final IProfilDevider devider = deviders[0];
        final DeviderEdit edit = new DeviderEdit( devider, IProfilDevider.DEVIDER_PROPERTY.BOESCHUNG, m_fzl_combo.getSelectionIndex() == 0 );

        final ProfilOperation operation = new ProfilOperation( "Lage der Trennfläche ändern", getProfilEventManager(), edit, true );
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
      public void widgetSelected( SelectionEvent e )
      {
        final IProfilDevider[] deviders = getProfil().getDevider( DEVIDER_TYP.TRENNFLAECHE );
        if( (deviders == null) || (deviders.length < 2) )
          return;
        final IProfilDevider devider = deviders[1];

        final DeviderEdit edit = new DeviderEdit( devider, IProfilDevider.DEVIDER_PROPERTY.BOESCHUNG, m_fzr_combo.getSelectionIndex() == 0 );

        final ProfilOperation operation = new ProfilOperation( "Lage der Trennfläche ändern", getProfilEventManager(), edit, true );
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
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setDeviderVisibility( DEVIDER_TYP.TRENNFLAECHE, m_fz_show.getSelection() );
        IProfilChange change = new VisibleDeviderEdit( getProfil(), DEVIDER_TYP.TRENNFLAECHE, m_fz_show.getSelection() );
        final ProfilChangeHint hint = new ProfilChangeHint();
        hint.setDeviderMoved();
        m_pem.fireProfilChanged( hint, new IProfilChange[] { change } );
      }
    } );

    final Group durchstroemteGroup = new Group( panel, SWT.NONE );
    durchstroemteGroup.setText( "Durchströmter Bereich" );
    durchstroemteGroup.setLayout( new GridLayout( 2, false ) );
    durchstroemteGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_dbl_text = new Text( durchstroemteGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbl_text.addModifyListener( doubleModifyListener );
    m_dbl_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {

        m_dbl_text.selectAll();

      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final double value = NumberUtils.parseQuietDouble( m_dbl_text.getText() );
        final IProfilDevider[] devs = getProfil().getDevider( DEVIDER_TYP.DURCHSTROEMTE );
        if( !Double.isNaN( value ) && devs != null && devs.length > 0 )
        {
          setValue( value, devs[0] );
        }
        else
          updateControls();
      }
    } );
    m_dbr_text = new Text( durchstroemteGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_dbr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_dbr_text.addModifyListener( doubleModifyListener );
    m_dbr_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {
        m_dbr_text.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {

        final double value = NumberUtils.parseQuietDouble( m_dbr_text.getText() );
        final IProfilDevider[] devs = getProfil().getDevider( DEVIDER_TYP.DURCHSTROEMTE );
        if( !Double.isNaN( value ) && devs != null && devs.length > 1 )
        {
          setValue( value, devs[1] );
        }
        else
          updateControls();
      }
    } );
    m_db_show = new Button( durchstroemteGroup, SWT.CHECK );
    final GridData db_showData = new GridData();
    db_showData.horizontalSpan = 2;
    m_db_show.setLayoutData( db_showData );
    m_db_show.setText( "Durchströmten Bereich anzeigen" );
    m_db_show.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setDeviderVisibility( DEVIDER_TYP.DURCHSTROEMTE, m_db_show.getSelection() );
        IProfilChange change = new VisibleDeviderEdit( getProfil(), DEVIDER_TYP.DURCHSTROEMTE, m_db_show.getSelection() );
        final ProfilChangeHint hint = new ProfilChangeHint();
        hint.setDeviderMoved();
        m_pem.fireProfilChanged( hint, new IProfilChange[] { change } );
      }
    } );

    final Group bordvollGroup = new Group( panel, SWT.NONE );
    bordvollGroup.setText( "Bordvollpunkte" );
    bordvollGroup.setLayout( new GridLayout( 2, false ) );
    bordvollGroup.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_bvl_text = new Text( bordvollGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvl_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvl_text.addModifyListener( doubleModifyListener );
    m_bvl_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {

        m_bvl_text.selectAll();

      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final double value = NumberUtils.parseQuietDouble( m_bvl_text.getText() );
        final IProfilDevider[] devs = getProfil().getDevider( DEVIDER_TYP.BORDVOLL );
        if( !Double.isNaN( value ) && devs != null && devs.length > 0 )
        {
          setValue( value, devs[0] );
        }
        else
          updateControls();
      }
    } );
    m_bvr_text = new Text( bordvollGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_bvr_text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_bvr_text.addModifyListener( doubleModifyListener );
    m_bvr_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {
        m_bvr_text.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {

        final double value = NumberUtils.parseQuietDouble( m_bvr_text.getText() );
        final IProfilDevider[] devs = getProfil().getDevider( DEVIDER_TYP.BORDVOLL );
        if( !Double.isNaN( value ) && devs != null && devs.length > 1 )
        {
          setValue( value, devs[1] );
        }
        else
          updateControls();
      }
    } );

    m_bv_show = new Button( bordvollGroup, SWT.CHECK );
    final GridData bv_showData = new GridData();
    bv_showData.horizontalSpan = 2;
    m_bv_show.setLayoutData( bv_showData );
    m_bv_show.setText( "Bordvollpunkte anzeigen" );
    m_bv_show.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setDeviderVisibility( DEVIDER_TYP.BORDVOLL, m_bv_show.getSelection() );
        IProfilChange change = new VisibleDeviderEdit( getProfil(), DEVIDER_TYP.BORDVOLL, m_bv_show.getSelection() );
        final ProfilChangeHint hint = new ProfilChangeHint();
        hint.setDeviderMoved();
        m_pem.fireProfilChanged( hint, new IProfilChange[] { change } );
      }
    } );

    m_bv_add = new Button( bordvollGroup, SWT.NONE );
    final GridData bv_addData = new GridData( GridData.FILL_HORIZONTAL );
    bv_addData.horizontalSpan = 2;
    m_bv_add.setLayoutData( bv_addData );
    m_bv_add.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        final IProfil profil = getProfil();
        final IProfilDevider[] bv_devs = profil.getDevider( IProfilDevider.DEVIDER_TYP.BORDVOLL );

        if( bv_devs == null )
        {
          final IProfilDevider[] db_devs = profil.getDevider( IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
          if( (db_devs == null) || (db_devs.length < 2) )
            return;
          final ProfilOperation operation = new ProfilOperation( "Bordvollpunkte einfügen:", getProfilEventManager(), true );
          operation.addChange( new DeviderAdd( profil, IProfilDevider.DEVIDER_TYP.BORDVOLL, db_devs[0].getPoint() ) );
          operation.addChange( new DeviderAdd( profil, IProfilDevider.DEVIDER_TYP.BORDVOLL, db_devs[1].getPoint() ) );
          operation.addChange( new ActiveObjectEdit( getProfil(), db_devs[1].getPoint(), null ) );
          new ProfilOperationJob( operation ).schedule();

        }
        else
        {
          final IProfilChange[] changes = new IProfilChange[2];
          changes[0] = new DeviderRemove( profil, bv_devs[0] );
          changes[1] = new DeviderRemove( profil, bv_devs[1] );
          final ProfilOperation operation = new ProfilOperation( "Bordvollpunkte entfernen:", getProfilEventManager(), changes, true );
          new ProfilOperationJob( operation ).schedule();

        }
      }
    } );

    updateControls();

    return panel;
  }

  /**
   * @exception SWTException
   *              <ul>
   *              <li>ERROR_THREAD_INVALID_ACCESS - if not called from the thread that created the receiver</li>
   *              </ul>
   */
  protected void updateControls( )
  {
    final IProfilDevider[] fz_devs = getProfil().getDevider( IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    final IProfilDevider[] db_devs = getProfil().getDevider( IProfilDevider.DEVIDER_TYP.DURCHSTROEMTE );
    final IProfilDevider[] bv_devs = getProfil().getDevider( IProfilDevider.DEVIDER_TYP.BORDVOLL );

    try
    {
      if( !m_fz_show.isDisposed() )
      {
        m_fz_show.setSelection( getViewData().getDeviderVisibility( DEVIDER_TYP.TRENNFLAECHE ) );
      }
      if( (!m_fzl_text.isDisposed()) && (fz_devs != null) && (fz_devs.length > 0) )
        m_fzl_text.setText( String.format( "%.4f", fz_devs[0].getPoint().getValueFor( POINT_PROPERTY.BREITE ) ) );
      else
        m_fzl_text.setText( "unbekannt" );
      if( !m_fzr_text.isDisposed() && (fz_devs != null) && (fz_devs.length > 1) )
        m_fzr_text.setText( String.format( "%.4f", fz_devs[1].getPoint().getValueFor( POINT_PROPERTY.BREITE ) ) );
      else
        m_fzr_text.setText( "unbekannt" );
      if( !m_fzl_combo.isDisposed() && (fz_devs != null) && (fz_devs.length > 0) )
      {
        if( (Boolean) fz_devs[0].getValueFor( IProfilDevider.DEVIDER_PROPERTY.BOESCHUNG ) )
        {
          m_fzl_combo.select( 0 );
        }
        else
        {
          m_fzl_combo.select( 1 );
        }
      }
      if( !m_fzr_combo.isDisposed() && (fz_devs != null) && (fz_devs.length > 1) )
      {
        if( (Boolean) fz_devs[1].getValueFor( IProfilDevider.DEVIDER_PROPERTY.BOESCHUNG ) )
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
        m_db_show.setSelection( getViewData().getDeviderVisibility( DEVIDER_TYP.DURCHSTROEMTE ) );
      }
      if( !m_dbl_text.isDisposed() && (db_devs != null) && (db_devs.length > 0) )
        m_dbl_text.setText( String.format( "%.4f", db_devs[0].getPoint().getValueFor( POINT_PROPERTY.BREITE ) ) );
      else
        m_dbl_text.setText( "unbekannt" );
      if( !m_dbr_text.isDisposed() && (db_devs != null) && (db_devs.length > 1) )
        m_dbr_text.setText( String.format( "%.4f", db_devs[1].getPoint().getValueFor( POINT_PROPERTY.BREITE ) ) );
      else
        m_dbr_text.setText( "unbekannt" );
      if( bv_devs == null )
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
          m_bv_show.setSelection( getViewData().getDeviderVisibility( DEVIDER_TYP.BORDVOLL ) );
        }
        if( !m_bv_add.isDisposed() )
        {
          m_bv_add.setText( "Bordvollpunkte entfernen" );
        }
        if( !m_bvl_text.isDisposed() && (bv_devs != null) && (bv_devs.length > 0) )
        {
          m_bvl_text.setVisible( true );
          m_bvl_text.setText( String.format( "%.4f", bv_devs[0].getPoint().getValueFor( POINT_PROPERTY.BREITE ) ) );

        }
        else
          m_bvl_text.setText( "unbekannt" );
        if( !m_bvr_text.isDisposed() && (bv_devs != null) && (bv_devs.length > 1) )
        {
          m_bvr_text.setVisible( true );
          m_bvr_text.setText( String.format( "%.4f", bv_devs[1].getPoint().getValueFor( POINT_PROPERTY.BREITE ) ) );
        }
        else
          m_bvr_text.setText( "unbekannt" );
      }
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
    }

  }

  protected void setValue( final double value, final IProfilDevider key )
  {
    if( Double.isNaN( value ) )
      return;

    final IProfilPoint pointCloseTo = getProfil().findNearestPoint( value );
    final IProfilPoint oldPos = key.getPoint();
    if( oldPos == pointCloseTo )
    {
      updateControls();
      return;
    }
    final ProfilOperation operation = new ProfilOperation( key.toString() + " verschieben", getProfilEventManager(), true );
    operation.addChange( new DeviderMove( key, pointCloseTo ) );
    operation.addChange( new ActiveObjectEdit( getProfil(), pointCloseTo, null ) );
    new ProfilOperationJob( operation ).schedule();

  }

  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( hint.isProfilPropertyChanged() || hint.isPointsChanged() || hint.isDeviderMoved() || hint.isDeviderDataChanged() )
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
}
