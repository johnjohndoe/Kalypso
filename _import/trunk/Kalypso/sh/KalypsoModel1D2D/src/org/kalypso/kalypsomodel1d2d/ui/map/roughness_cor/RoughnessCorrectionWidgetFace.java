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
package org.kalypso.kalypsomodel1d2d.ui.map.roughness_cor;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;

/**
 * Face for roughness correction widget
 * 
 * @author Patrice Congo
 *
 */
public class RoughnessCorrectionWidgetFace
{
  public static final String HANDLE_WIDTH_NAME = "x.handleWidth";
  
  private Composite rootPanel;
  private FormToolkit toolkit;
  


  /**
   * Section that holds the finite element style
   * control user interface
   */
  private Section styleControlSection;
  
  /**
   * Section that holds the user interface for modelling
   * roughness correction
   */
  private Section modellingSection;

  /**
   * Hold data for the roughness widget and its face
   */
  private RoughnessCorrectionDataModel dataModel;
  
  /**
   * Provide the user interface for controlling
   * ths display of element roughness styling
   */
  private RoughnessStylingControl stylingControl;

 
  public RoughnessCorrectionWidgetFace( 
                  RoughnessCorrectionDataModel feDataModel )
  {
   this.dataModel = feDataModel;   
  }


  
  public Control createControl( Composite parent )
  {
    initStoreDefaults();

    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    toolkit = new FormToolkit( parent.getDisplay() );
    ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );
    ////************ global layout ****************************/////
    // Creates Section for "Element roughness styling section
    styleControlSection = 
              toolkit.createSection( 
                      scrolledForm.getBody(), 
                      Section.TREE_NODE | Section.CLIENT_INDENT | 
                        Section.TWISTIE | Section.DESCRIPTION | 
                        Section.TITLE_BAR );
    styleControlSection.setText( "Einstellung zur Rauheitsanzeige" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    styleControlSection.setLayoutData( tableWrapData );
    styleControlSection.setExpanded( true );
    // Element wise roughness correction
    Section eleWiseSection = 
              toolkit.createSection( 
                  scrolledForm.getBody(), 
                  Section.TREE_NODE | Section.CLIENT_INDENT | 
                    Section.TWISTIE | Section.DESCRIPTION | 
                    Section.TITLE_BAR );
    eleWiseSection.setText( "Elementweise Korrektur" );
    tableWrapData = 
        new TableWrapData( 
              TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    eleWiseSection.setLayoutData( tableWrapData );
    eleWiseSection.setExpanded( false );
    eleWiseSection.setEnabled( true );
    
    //sub controls
    stylingControl =
      new RoughnessStylingControl(
                    dataModel,toolkit,styleControlSection);
    stylingControl.createControl();

    ElWiseRoughnessCorrectionControl eleWiseCC =
      new ElWiseRoughnessCorrectionControl(
                dataModel, toolkit, eleWiseSection);
    eleWiseCC.createControl();
    
    return rootPanel;
  }



  
  public void disposeControl( )
  {
    if( stylingControl != null )
    {
      stylingControl.dispose();
    }
    
    if( rootPanel == null )
    {
      System.out.println( "Disposing null root panel" );
      return;
    }
    if( !rootPanel.isDisposed() )
    {
      rootPanel.dispose();
      toolkit.dispose();
    }
   
  }
  
  
 
  private void initStoreDefaults( )
  {

  }
  
   private IPropertyChangeListener createPropertyChangeLis( )
  {
    return new IPropertyChangeListener()
    {

      public void propertyChange( PropertyChangeEvent event )
      {
        Object source = event.getSource();
        String property = event.getProperty();

        if( source instanceof FieldEditor )
        {
          ((FieldEditor) source).store();
        }
        else if( source instanceof ColorSelector )
        {
        }
        else
        {
          System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() );
        }
      }

    };
  }
   
}
