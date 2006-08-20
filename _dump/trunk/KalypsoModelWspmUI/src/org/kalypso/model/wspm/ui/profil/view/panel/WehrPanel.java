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
package org.kalypso.model.wspm.ui.profil.view.panel;

import java.util.LinkedList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
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
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.ProfilDeviderFactory;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_TYP;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.BuildingEdit;
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
 * @author belger
 */
/**
 * @author kimwerner
 */
public class WehrPanel extends AbstractProfilView
{

  private class DeviderLine extends Composite
  {
    protected IProfilDevider m_devider;

    private int position;

    protected Text m_beiwert;

    protected Text m_point;

    private Button m_buttonD;

    public DeviderLine( final int index )
    {
      super( m_deviderGroup, SWT.NONE );
      position = index;

      setLayout( new GridLayout( 4, false ) );
      final GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
      gridData.horizontalSpan = 4;
      setLayoutData( gridData );

      final Display display = m_deviderGroup.getDisplay();
      final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
      final Color badColor = display.getSystemColor( SWT.COLOR_RED );
      final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

      final Label label1 = new Label( this, SWT.NONE );
      label1.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
      label1.setText( "Feld " + Integer.toString( position + 1 ) + ":" );

      m_beiwert = new Text( this, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
      m_beiwert.setLayoutData( new GridData( GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL ) );
      m_beiwert.addModifyListener( doubleModifyListener );
      m_beiwert.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( FocusEvent e )
        {
          m_beiwert.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          final double value = NumberUtils.parseQuietDouble( m_beiwert.getText() );
          if( !Double.isNaN( value ) )
          {
            final IProfilChange[] changes = new IProfilChange[2];
            changes[0] = new DeviderEdit( m_devider, IProfilDevider.DEVIDER_PROPERTY.BEIWERT, value );
            changes[1] = new ActiveObjectEdit( getProfil(), m_devider.getPoint(), null );
            final ProfilOperation operation = new ProfilOperation( "Wehrfeldparameter ändern", getProfilEventManager(), changes, true );
            new ProfilOperationJob( operation ).schedule();

          }
        }
      } );

      m_point = new Text( this, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
      m_point.setLayoutData( new GridData( GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL ) );
      m_point.addModifyListener( doubleModifyListener );
      m_point.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( FocusEvent e )
        {
          m_point.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          final double value = NumberUtils.parseQuietDouble( m_point.getText() );
          if( !Double.isNaN( value ) )
          {
            final IProfilPoint point = getProfil().findNearestPoint( value );
            if( point != m_devider.getPoint() )
            {

              final IProfilChange[] changes = new IProfilChange[2];
              changes[0] = new DeviderMove( m_devider, point );
              changes[1] = new ActiveObjectEdit( getProfil(), point, null );
              final ProfilOperation operation = new ProfilOperation( "Wehrfeld verschieben", getProfilEventManager(), changes, true );
              new ProfilOperationJob( operation ).schedule();
            }
          }
        }
      } );

      m_buttonD = new Button( this, SWT.NONE );
      m_buttonD.setImage( m_deleteImg );
      m_buttonD.setToolTipText( "löscht den linken Wehrfeldtrenner" );
      m_buttonD.addSelectionListener( new SelectionAdapter()
      {

        @Override
        public void widgetSelected( SelectionEvent e )
        {
          final IProfilChange change = new DeviderRemove( getProfil(), m_devider );
          final ProfilOperation operation = new ProfilOperation( "Wehrfeld löschen", getProfilEventManager(), change, true );
          new ProfilOperationJob( operation ).schedule();
        }
      } );
    }

    public void refresh( final IProfilDevider dev )
    {
      m_devider = dev;
      final Double value = dev == null ? 0.0 : (Double) dev.getValueFor( IProfilDevider.DEVIDER_PROPERTY.BEIWERT );
      m_beiwert.setText( value.toString() );
      try
      {
        m_point.setText( Double.toString( dev.getPoint().getValueFor( POINT_PROPERTY.BREITE ) ) );
      }
      catch( final ProfilDataException e )
      {
        // do nothing
      }
    }

    @Override
    public void dispose( )
    {
      super.dispose();
      m_buttonD.dispose();
      m_beiwert.dispose();
    }
  }

  private Combo m_Wehrart;

  protected Button m_WehrfeldVisible;

  protected Group m_deviderGroup;

  protected Text m_kronenParameter;

  protected Button m_DeviderAdd;

  private LinkedList<DeviderLine> m_deviderLines;

  protected final Image m_deleteImg;

  private final Image m_addImg;

  public WehrPanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata, null );
    m_deviderLines = new LinkedList<DeviderLine>();
    // TODO: KIM images suchen
    m_deleteImg = null;// KalypsoModelWspmUIImages.ID_BUTTON_WEHR_DELETE.createImage();
    m_addImg = null;// KalypsoModelWspmUIImages.ID_BUTTON_WEHR_ADD.createImage();
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    m_deleteImg.dispose();
    m_addImg.dispose();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, false );
    panel.setLayout( gridLayout );

    // Wehrart ComboBox
    final String tooltip = IProfilBuilding.BUILDING_PROPERTY.WEHRART.getTooltip();
    final Label label = new Label( panel, SWT.NONE );
    label.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    label.setText( "Wehrart:" );
    label.setToolTipText( tooltip );
    m_Wehrart = new Combo( panel, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_Wehrart.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    for( IProfil.WEHR_TYP wt : IProfil.WEHR_TYP.values() )
    {
      m_Wehrart.add( wt.toString() );
    }
    m_Wehrart.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        int i = m_Wehrart.getSelectionIndex();
        IProfil.WEHR_TYP wt = IProfil.WEHR_TYP.values()[i];
        IProfilChange change = new BuildingEdit( getProfil().getBuilding(), BUILDING_PROPERTY.WEHRART, wt );
        final ProfilOperation operation = new ProfilOperation( "Wehrart ändern", getProfilEventManager(), change, true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    // Wehrparameter Group
    m_deviderGroup = new Group( panel, SWT.NONE );
    m_deviderGroup.setText( "Wehrfeldtrenner" );
    final GridLayout groupLayout = new GridLayout( 4, false );
    m_deviderGroup.setLayout( groupLayout );
    final GridData groupData = new GridData( GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL | GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL );
    groupData.horizontalSpan = 2;
    m_deviderGroup.setLayoutData( groupData );

    // Zeile 1
    final Label l5 = new Label( m_deviderGroup, SWT.NONE );
    l5.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    l5.setText( "Feld 1: " );
    m_kronenParameter = new Text( m_deviderGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_kronenParameter.setLayoutData( new GridData( GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL ) );
    m_kronenParameter.addModifyListener( new DoubleModifyListener( panel.getDisplay().getSystemColor( SWT.COLOR_BLACK ), panel.getDisplay().getSystemColor( SWT.COLOR_RED ) ) );
    m_kronenParameter.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {
        m_kronenParameter.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final double value = NumberUtils.parseQuietDouble( m_kronenParameter.getText() );
        if( !Double.isNaN( value ) )
        {
          final IProfilChange change = new BuildingEdit( getProfil().getBuilding(), BUILDING_PROPERTY.FORMBEIWERT, value );
          final ProfilOperation operation = new ProfilOperation( "Wehrfeldparameter ändern", getProfilEventManager(), change, true );
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );
    new Label( m_deviderGroup, SWT.NONE );
    m_DeviderAdd = new Button( m_deviderGroup, SWT.NONE );
    m_DeviderAdd.setImage( m_addImg );
    m_DeviderAdd.setToolTipText( "fügt einen neuen Wehrfeldtrenner ein" );
    m_DeviderAdd.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        try
        {
          final IProfilBuilding building = getProfil().getBuilding();
          if( (building == null) || (building.getTyp() != BUILDING_TYP.WEHR) )
          {
            return;
          }
          final double wehrParameter = (Double) building.getValueFor( BUILDING_PROPERTY.FORMBEIWERT );
          final IProfilDevider[] trennFl = getProfil().getDevider( DEVIDER_TYP.TRENNFLAECHE );
          final IProfilPoint point = trennFl[0].getPoint();
          final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.WEHR, point );
          final IProfilChange[] changes = new IProfilChange[3];
          changes[0] = new DeviderAdd( getProfil(), devider );
          changes[1] = new DeviderEdit( devider, IProfilDevider.DEVIDER_PROPERTY.BEIWERT, wehrParameter );
          changes[2] = new ActiveObjectEdit( getProfil(), point, null );
          final ProfilOperation operation = new ProfilOperation( "Wehrfeld erzeugen", getProfilEventManager(), changes, true );
          new ProfilOperationJob( operation ).schedule();
        }
        catch( ProfilDataException ex )
        {
          // do nothing
        }
      }
    } );

    // Zeile 2
    new Label( m_deviderGroup, SWT.NONE );
    final Label l2 = new Label( m_deviderGroup, SWT.NONE );
    l2.setText( "Parameter" );

    final Label l3 = new Label( m_deviderGroup, SWT.NONE );
    l3.setText( "Position" );
    new Label( m_deviderGroup, SWT.NONE );

    // Wehrfeldsichtbar Check
    m_WehrfeldVisible = new Button( panel, SWT.CHECK );
    final GridData checkData = new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL );
    checkData.horizontalSpan = 2;
    m_WehrfeldVisible.setLayoutData( checkData );
    m_WehrfeldVisible.setText( "Feldtrenner anzeigen" );
    m_WehrfeldVisible.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setDeviderVisibility( DEVIDER_TYP.WEHR, m_WehrfeldVisible.getSelection() );
        IProfilChange change = new VisibleDeviderEdit( getProfil(), DEVIDER_TYP.WEHR, m_WehrfeldVisible.getSelection() );
        final ProfilChangeHint hint = new ProfilChangeHint();
        hint.setDeviderMoved();
        m_pem.fireProfilChanged( hint, new IProfilChange[] { change } );
        // getViewData().setDeviderVisibility( DEVIDER_TYP.WEHR, m_WehrfeldVisible.getSelection() );
      }
    } );

    updateControls();

    return panel;
  }

  protected void updateControls( )
  {
    try
    {
      if( m_Wehrart.isDisposed() )
        return;
      final IProfilBuilding building = getProfil().getBuilding();
      if( building == null )
        return;
      IProfil.WEHR_TYP wt = (IProfil.WEHR_TYP) building.getValueFor( BUILDING_PROPERTY.WEHRART );
      m_Wehrart.select( m_Wehrart.indexOf( wt.toString() ) );
      m_WehrfeldVisible.setSelection( getViewData().getDeviderVisibility( DEVIDER_TYP.WEHR ) );
      m_kronenParameter.setText( building.getValueFor( BUILDING_PROPERTY.FORMBEIWERT ).toString() );
      final IProfilDevider[] deviders = getProfil().getDevider( DEVIDER_TYP.WEHR );
      final int deviderCount = (deviders == null ? 0 : deviders.length);
      {
        while( m_deviderLines.size() < deviderCount )
        {
          m_deviderLines.add( new DeviderLine( m_deviderLines.size() + 1 ) );
        }
        while( m_deviderLines.size() > deviderCount )
        {
          m_deviderLines.getLast().dispose();
          m_deviderLines.removeLast();
        }
        int index = 0;
        for( DeviderLine devl : m_deviderLines )
        {
          devl.refresh( deviders[index++] );
        }
      }
      m_deviderGroup.layout();
    }
    catch( final ProfilDataException e )
    {
      // do nothing
    }
  }

  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( hint.isBuildingDataChanged() || hint.isProfilPropertyChanged() || hint.isDeviderMoved() || hint.isDeviderDataChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
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
