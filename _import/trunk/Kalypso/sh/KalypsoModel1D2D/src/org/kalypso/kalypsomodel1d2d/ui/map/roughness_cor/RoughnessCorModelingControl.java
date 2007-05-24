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
package org.kalypso.kalypsomodel1d2d.ui.map.roughness_cor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1D2DElementRoughnessTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsomodel1d2d.update.ModelMergeService;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * Provide the ui for controling the roughness styling of finite elements
 * 
 * @author Patrice Congo
 * 
 */
public class RoughnessCorModelingControl
{

  /**
   * Check button to de- or activate element roughness styling area
   */
  private Button activateButton;

  private final RoughnessCorrectionDataModel dataModel;

  private final FormToolkit toolkit;

  private final Section parentSection;

  /**
   * Group provide ui to choose the correction modus polygon base or point base
   */
  private Group corModusGroup;

  private Group queryGroup;

  private Group corGroup;

  RoughnessCorModelingControl( final RoughnessCorrectionDataModel dataModel, final FormToolkit toolkit, final Section parentSection )
  {
    this.dataModel = dataModel;
    this.toolkit = toolkit;
    this.parentSection = parentSection;
  }

  public void createControl( )
  {
    createFEStylingControlModel( parentSection );
  }

  public void dispose( )
  {

  }

  /**
   * Creates a control which provide the interface to control to roughness styling of elements
   * 
   * @param the
   *            parent section which provides client area to hold the styling control ui
   * 
   */
  private final void createFEStylingControlModel( final Section workStatusSection )
  {

    workStatusSection.setLayout( new FillLayout() );

    final Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    // clientComposite.setSize( 400, 300 );
    final FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 5 );
    clientComposite.setLayoutData( formData );

    activateButton = new Button( clientComposite, SWT.CHECK/* PUSH */);
    formData = new FormData();
    formData.top = new FormAttachment( 0, 5 );
    formData.left = new FormAttachment( 0, 5 );
    activateButton.setText( "Rauheitsanzeige für 2D-Elemente aktivieren" );
    activateButton.setLayoutData( formData );
    final ModelMergeService mergeService = ModelMergeService.getInstance();
    // TODO Patrice put this into preferences or ..
    final boolean isRoughnessToBeDisplay = mergeService.getIsRoughnessToBeDisplay();
    activateButton.setSelection( isRoughnessToBeDisplay );
    toggleMap( isRoughnessToBeDisplay );

    activateButton.addSelectionListener( makeActivateButtonSelectionListener() );

    final Composite newComposite = new Composite( clientComposite, SWT.None );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( activateButton, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    newComposite.setLayoutData( formData );
    newComposite.setLayout( new GridLayout( 1, false ) );

  }

  public final void toggleMap( final boolean selected )
  {

    final ModelMergeService mergeService = ModelMergeService.getInstance();

    mergeService.doReInit();
    mergeService.setIsRoughnessToBeDisplay( selected );

    final IMapModell mapModell = dataModel.getMapModell();
    Model1D2DElementRoughnessTheme theme = UtilMap.findTheme( mapModell, Model1D2DElementRoughnessTheme.class );
    if( theme == null )
    {
      theme = new Model1D2DElementRoughnessTheme( "Element+Rauhheiten", mapModell );
      mapModell.addTheme( theme );
    }
    if( selected )
    {
      final IStaticModel1D2D staticModel = Util.getModel( IStaticModel1D2D.class );
      theme.setStaticModel( staticModel );
    }
    else
    {
      mapModell.removeTheme( theme );// theme.setStaticModel( null );
    }
    mapModell.invalidate( null );
  }

  private final SelectionListener makeActivateButtonSelectionListener( )
  {
    return new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        toggleMap( isStylingOptionChecked() );
      }
    };
  }

  public final boolean isStylingOptionChecked( )
  {
    return activateButton.getSelection();
  }
}
