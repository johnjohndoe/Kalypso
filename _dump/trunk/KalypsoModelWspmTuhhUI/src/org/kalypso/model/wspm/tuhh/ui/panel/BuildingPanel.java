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

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;

/**
 * @author belger
 */
/**
 * @author kimwerner
 */
public class BuildingPanel extends AbstractProfilView
{
  private final Collection<Text> m_texts = new ArrayList<Text>( 8 );

  private final IProfileObject m_building;

  private String getLabel( final String property )
  {
    return m_building.getLabelFor( property );
    // TODO: Kim return getLabelProvider.getCaptionFor(property);
    // if( property == BUILDING_PROPERTY.BREITE )
    // {
    // final String buildigTyp = m_building.getTyp();
    // if( buildigTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_EI ) == 0 )
    // {
    // return "grösster Durchmesser [m]";
    // }
    // else if( buildigTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) == 0 )
    // {
    // return "lange Seite [m]";
    // }
    // else if( buildigTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) == 0 )
    // {
    // return "Breite in Fließrichtung [m]";
    // }
    // else
    // return property.toString();
    // }
    //return property;
  }

  public BuildingPanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata );

    m_building = getProfil().getProfileObject();
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
    gridLayout.verticalSpacing = 15;
    panel.setLayout( gridLayout );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Display display = parent.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    for( final String buildingProperty : m_building.getObjectProperties() )
    {
      final String tooltip = ""; // TODO: Kim labelProvider.getTooltipFor(buildingProperty);

      final Label label = new Label( panel, SWT.NONE );
      label.setLayoutData( new GridData( GridData.BEGINNING, GridData.BEGINNING, true, false ) );
      label.setText( getLabel( buildingProperty ) );
      label.setToolTipText( tooltip );

      final Text bldText = new Text( panel, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
      m_texts.add( bldText );
      bldText.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

      bldText.addModifyListener( doubleModifyListener );
      bldText.setData( buildingProperty );
      bldText.setToolTipText( tooltip );
      bldText.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( FocusEvent e )
        {
          bldText.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          final double value = NumberUtils.parseQuietDouble( bldText.getText() );
          final IProfil profil = getProfil();
          final IProfileObject building = profil.getProfileObject();
          if( Double.isNaN( value ) || (building == null) )
          {
            updateControls();
            return;
          }
          try
          {
            final double currentValue = (Double) building.getValueFor( buildingProperty );
            if( value == currentValue )
              return;
            final ProfileObjectEdit edit = new ProfileObjectEdit( profil.getProfileObject(), buildingProperty, value );
            final ProfilOperation operation = new ProfilOperation( /* TODO: labelProvider */"" + " ändern", getProfilEventManager(), edit, true );
            new ProfilOperationJob( operation ).schedule();
          }
          catch( Exception e1 )
          {
            updateControls();
          }
        }
      } );
    }

    updateControls();

    return panel;
  }

  protected void updateControls( )
  {
    for( final Text text : m_texts )
    {
      try
      {
        if( !text.isDisposed() )
        {
          text.setText( String.format( "%.2f", m_building.getValueFor( text.getData().toString() ) ) );
          if( text.isFocusControl() )
            text.selectAll();
        }
      }
      catch( final Exception e )
      {
        text.setText( "Formatfehler" );
      }
    }
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isObjectChanged() || hint.isObjectDataChanged() )
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