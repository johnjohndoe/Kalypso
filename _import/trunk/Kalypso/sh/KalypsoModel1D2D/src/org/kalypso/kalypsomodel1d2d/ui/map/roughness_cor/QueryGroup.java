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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledPageBook;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.kalypsomodel1d2d.merge.IRoughnessBasedElementTest;
import org.kalypso.kalypsomodel1d2d.merge.ITest;
import org.kalypso.kalypsomodel1d2d.merge.RoughnessMergeServiceBasedElementTest;
import org.kalypso.kalypsomodel1d2d.merge.Selector;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1D2DElementRoughnessTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsomodel1d2d.update.ModelMergeService;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

import sun.management.MXBeanSupport;
import sun.reflect.generics.tree.BottomSignature;

/**
 * Provide the ui to select query type, specify it and 
 * perform the query
 * 
 * @author Patrice Congo
 *
 */
public class QueryGroup
{
  
  /**
   * Check button to de- or activate element roughness
   * styling area
   */
  private Button activateButton;
  
  private final RoughnessCorrectionDataModel dataModel;
  
  private final FormToolkit toolkit;
  
  private final Composite parentSection;

  private final RoughnessMergeServiceBasedElementTest tester =
                  new RoughnessMergeServiceBasedElementTest();

  private ScrolledPageBook specBook;

  private ComboViewer comboViewer;
  
  public QueryGroup(
              RoughnessCorrectionDataModel dataModel,
              FormToolkit toolkit, 
              Composite parentSection )
  {
    this.dataModel = dataModel;
    this.toolkit = toolkit;
    this.parentSection = parentSection;    
  }
  
  public Control createControl()
  {
    return createQueryGoup( parentSection );
  }
  
  public void dispose()
  {
    
  }
  
  /**
   * Creates a control which provide the interface to
   * control to roughness styling of elements
   * @param the parent section which provides client area
   *            to hold the styling control ui 
   *    
   */
  private final Control createQueryGoup( 
                          Composite workStatusSection )
  {
    
    Group clientComposite =
        new Group(workStatusSection,SWT.FLAT);
    clientComposite.setText( "Selection" );
    toolkit.adapt( clientComposite );
//    Composite clientComposite = 
//        toolkit.createComposite( workStatusSection, SWT.FLAT );
    
    FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );

    FormData formData ;//
    
    //label
    Label labelModus = 
      toolkit.createLabel( clientComposite, "Modus:" );
    formData= new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
//    formData.bottom = new FormAttachment( 100, 5 );
    labelModus.setLayoutData( formData );
    Label sepModus = 
        toolkit.createSeparator( clientComposite, SWT.HORIZONTAL );
    formData= new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.right = new FormAttachment( 95, 0 );
    formData.top = new FormAttachment( labelModus, 5 );
    sepModus.setLayoutData( formData );
    
    //type selection combo    
    comboViewer = new ComboViewer( clientComposite );
    formData= new FormData();
    formData.left = new FormAttachment(10, 0 );
    formData.top = new FormAttachment( sepModus, 5 );
    final Combo combo = comboViewer.getCombo();
    //    formData.bottom = new FormAttachment( 100, 5 );
    combo.setLayoutData( formData );
    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setInput( 
        IRoughnessBasedElementTest.SELECTION_CONDITION.values() );
    makeComboViewSelectionListener(  );
    //label specification
    Label labelSpec = 
      toolkit.createLabel( clientComposite, "Spezifikation" );
    formData= new FormData();
    formData.left = new FormAttachment(0, 5 );
    formData.top = new FormAttachment( combo, 5 );
    labelSpec.setLayoutData( formData );
    //sep
    Label sepSpec = 
        toolkit.createSeparator( clientComposite, SWT.HORIZONTAL );
    formData= new FormData();
    formData.left = new FormAttachment(0, 5 );
    formData.right = new FormAttachment(95, 0 );
    formData.top = new FormAttachment( labelSpec, 5 );
    sepSpec.setLayoutData( formData );
    //spec area
    specBook = 
      toolkit.createPageBook( clientComposite, SWT.NONE );
    formData= new FormData();
    formData.left = new FormAttachment(10, 5 );
    formData.right = new FormAttachment( 95, 0 );
    formData.top = new FormAttachment( sepSpec, 5 );
    formData.bottom = new FormAttachment( 95, 0 );
    formData.height = 100;
    specBook.setLayoutData( formData );
    addNoRoughnessPage( specBook );
    addMaxRoughnessPage( specBook );
    specBook.showEmptyPage();  
    return clientComposite;
  }
  
  private final void addNoRoughnessPage( ScrolledPageBook book )
  {
    Composite composite = book.createPage( 
        IRoughnessBasedElementTest.SELECTION_CONDITION.NO_ROUGHNESS );
    composite.setLayout(  new FillLayout() );
    Button button = 
      toolkit.createButton( composite, "Durchführen", SWT.FLAT );

    final SelectionListener listener = 
      new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected( SelectionEvent e )
        {
//          final IWorkbench wb = PlatformUI.getWorkbench();
          final ITest<IFE1D2DElement> noRoughnessTest = 
              new ITest<IFE1D2DElement>()
              {

                public boolean doesPass( IFE1D2DElement candidate )
                {
                  boolean res = tester.satisfies( 
                      candidate, 
                      IRoughnessBasedElementTest.SELECTION_CONDITION.NO_ROUGHNESS , 
                      null );
                  return res;
                }
            
              };
              makeElementSelection( 
                  "Suche nach element ohne Rauhheit", 
                  noRoughnessTest );      
        }
      };
    button.addSelectionListener( listener  );
  }
  
  final void makeElementSelection(
                        final String selectionInfo,
                        final ITest<IFE1D2DElement> noRoughnessTest )
  {
    final IWorkbench wb = PlatformUI.getWorkbench();
    final IFEDiscretisationModel1d2d model = Util.getModel( IFEDiscretisationModel1d2d.class );
    final IFeatureWrapperCollection<IFE1D2DElement> elements = model.getElements();     
    Selector<IFE1D2DElement> selector =
       new Selector<IFE1D2DElement>(
             selectionInfo,noRoughnessTest,elements);
    try
    {
      wb.getProgressService().run( false, true, selector );
      Collection<IFE1D2DElement> selected = selector.getSelected();
      dataModel.setSelectedElements( selected  );
    }
    catch( InvocationTargetException e1 )
    {
      e1.printStackTrace();
    }
    catch( InterruptedException e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }
   
  }
  
  private final void addMaxRoughnessPage( ScrolledPageBook book )
  {
    Composite composite = book.createPage( 
        IRoughnessBasedElementTest.SELECTION_CONDITION.MAX_PERCENTAGE_MOSTSPREAD );
    composite.setLayout(  new FillLayout() );
    Button button = 
      toolkit.createButton( composite, "Durchführen", SWT.FLAT );
    final SelectionListener listener = 
      new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected( SelectionEvent e )
        {
          ITest<IFE1D2DElement> maxRoughnessTest = 
            new ITest<IFE1D2DElement>()
          {

            public boolean doesPass( IFE1D2DElement candidate )
            {
              boolean result = tester.satisfies( 
                  candidate, 
                  IRoughnessBasedElementTest.SELECTION_CONDITION.MAX_PERCENTAGE_MOSTSPREAD , 
                  0.5 );
              return result;
            }
            
          };
          
          makeElementSelection( 
              "Suche nach Elemente mit einer maximale Abdeckung für"+
                " eine Rauheit kleiner als:"+0.5, 
              maxRoughnessTest );
       
        }
      };
    button.addSelectionListener( listener  );
  }
  
  
  private final void makeComboViewSelectionListener(  )
  {
    ISelectionChangedListener listener = new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        ISelection selection = event.getSelection();
        if( selection instanceof IStructuredSelection )
        {
          Object firstElement = 
            ((IStructuredSelection)selection).getFirstElement();
          
          if( specBook.hasPage( firstElement ) )
          {
            System.out.println("One Page:"+firstElement);
            specBook.showPage( firstElement );
          }
          else
          {
            System.out.println("No Page:"+firstElement);
            specBook.showEmptyPage();
          }
          specBook.redraw();
          parentSection.redraw();
          Composite parent = parentSection.getParent();
          parent.redraw();
          Composite parent2 = parent.getParent();
          parent2.redraw();
          parent2.pack();//update();
        }
      }      
    };
    comboViewer.addSelectionChangedListener( listener );
  }
  
 }
