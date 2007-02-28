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
package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.ScrolledPageBook;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;

class DeleteFEElementsWidgetFace
  {
  
  


    private Composite rootPanel;
    private FormToolkit toolkit;
    static private IPreferenceStore preferenceStore = 
          KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();
    
    private WidgetStrategyContext strategyContext;
    private ToolBar toolBar ;
    private ScrolledPageBook book;
    private List< Image> images= new ArrayList<Image>();
    
    public DeleteFEElementsWidgetFace(
                      WidgetStrategyContext strategyContext)
    {
      this.strategyContext=strategyContext;
    }
    
    public Control createControl( Composite parent )
    {      
      parent.setLayout( new FillLayout() );
      rootPanel=new Composite(parent, SWT.FILL);
      rootPanel.setLayout( new FillLayout() );
      toolkit= new FormToolkit(parent.getDisplay());
      ScrolledForm scrolledForm = 
                toolkit.createScrolledForm( rootPanel );
      
      Composite body = scrolledForm.getBody();
      body.setLayout(new FillLayout());
      Composite clientComposite = 
        toolkit.createComposite( body, SWT.NONE );
      
      clientComposite.setLayout( new FormLayout() );
      
      toolBar = new ToolBar(clientComposite, SWT.INHERIT_FORCE);
//      CoolBar coolBar= new CoolBar(clientComposite,SWT.NONE);
      FormData formData= new FormData();
      formData.left = new FormAttachment(0,1,0);
      formData.right = new FormAttachment(100,1,0);
      formData.top = new FormAttachment(0,1,0);
//      formData.height = 32;
      toolBar.setLayoutData( formData );
      
      //item
//      final ToolItem item1 = 
//              new ToolItem(toolBar, SWT.PUSH);
      Image image = getImage( "/icons/viewmag1.png" );
      
//      ToolBarManager manager= new ToolBarManager();
//      toolBar = manager.createControl( clientComposite);
//      FormData formData= new FormData();
//      formData.left=new FormAttachment(0,1,0);
//      formData.right=new FormAttachment(100,1,0);
//      formData.top=new FormAttachment(0,1,0);
//      formData.height=32;
//      toolBar.setLayoutData( formData );
      
      final Action action= new Action("dadadad")
      {
        final private FeElementPointSelectionWidget pointSelectionWidget=
          new FeElementPointSelectionWidget();

        /**
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run( )
        {
          book.showPage("_POINT_CLICK_SEL_");
          strategyContext.setStrategy( pointSelectionWidget);
        }
      };
      
      
      final Action actionSelNode= new Action("SelNode")
      {
        final private FENetConceptSelectionWidget nodeSelectionWidget=
          new FENetConceptSelectionWidget(
              Kalypso1D2DSchemaConstants.WB1D2D_F_NODE);

        /**
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run( )
        {
          book.showEmptyPage();
          strategyContext.setStrategy( nodeSelectionWidget );
        }
        
      };
      
      try
      {
        action.setText( "dadad" );
        action.setToolTipText( 
            "Selectieren ein Finite element durch klicken" );
        action.setImageDescriptor( 
            getImageDescriptor( "/icons/viewmag1.png" ));
        ActionContributionItem aci = 
                new ActionContributionItem(action);
        aci.fill( toolBar, 0 );  
        
        images.add( image );
        
        ActionContributionItem aciNodeSel = 
            new ActionContributionItem(actionSelNode);
       actionSelNode.setToolTipText( "Select nodes" );
        aciNodeSel.fill( toolBar, 1 );
        
        toolkit.adapt( toolBar, true, true );   
        toolkit.paintBordersFor( toolBar );
        
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      
      
      //page book
      book = toolkit.createPageBook( clientComposite, SWT.NONE );    
      formData= new FormData();
      formData.left=new FormAttachment(0,1,0);
      formData.right=new FormAttachment(100,1,0);
      formData.top= new FormAttachment(toolBar);
      formData.bottom=new FormAttachment(100,1,0);
      book.setLayoutData( formData );
      
//      //book page
      final String klickSeclectKey ="_POINT_CLICK_SEL_";
      //add book page
      Composite composite = book.createPage( klickSeclectKey );
      composite.setLayout( new FillLayout() );
      Text text = toolkit.createText( 
                      composite, "Click dsded Select Page" );

//      item1.addSelectionListener(new SelectionAdapter() {
//        
//        public void widgetSelected(final SelectionEvent e) {
//          book.showPage( klickSeclectKey );
//          System.out.println("Shwoing:"+klickSeclectKey+
//              "\n\tvisible="+book.getVisible()+
//              "\n\thas_currentPage="+book.hasPage( klickSeclectKey )+
//              "\n\tcurrentPage="+book.getCurrentPage());
//        }
//       });
      
     
//      rootPanel.layout();
      addPointSelection();
//      rootPanel.layout();
      book.showEmptyPage();
      return rootPanel;
    }
    

    
    public void disposeControl( )
    {
      if(rootPanel==null)
      {
        return;
      }
      if(!rootPanel.isDisposed())
      {
        rootPanel.dispose();
        toolkit.dispose();
      }
      for(Image image:images)
      {
        if(image.isDisposed())
        {
          image.dispose();
        }
      }
      images.clear();
    }
    

    
    private final void addPointSelection()
    {
      
//      final ToolItem item1 = new ToolItem(toolBar, SWT.PUSH);
//      Image image = getImage( "/icons/viewmag1.png" );
//      images.add( image );
//      item1.setImage( image );
//      item1.setText( "Selection by clicking" );
      
//      final String klickSeclectKey ="_POINT_CLICK_SEL_";
//      //add book page
//      Composite composite = book.createPage( klickSeclectKey );
//      composite.setLayout( new FillLayout() );
//      Text text = toolkit.createText( composite, "Click dsded Select Page" );
//
//      item1.addSelectionListener(new SelectionAdapter() {
//        
//        public void widgetSelected(final SelectionEvent e) {
//          book.showPage( klickSeclectKey );
//          System.out.println("Shwoing:"+klickSeclectKey+
//              "\n\tvisible="+book.getVisible()+
//              "\n\thas_currentPage="+book.hasPage( klickSeclectKey )+
//              "\n\tcurrentPage="+book.getCurrentPage());
//        }
//       });
      
//      toolBar.pack();
//      book.pack();
    }
    
    
    
    
    
    
    private final void createHelp(Section helpSection)
    {
      helpSection.setLayout( new FillLayout());
      
      Composite clientComposite = 
              toolkit.createComposite( helpSection , SWT.FLAT);
      helpSection.setClient( clientComposite );
      clientComposite.setLayout( new FillLayout() );
      Browser browser= new Browser(clientComposite,SWT.FLAT|SWT.FILL_EVEN_ODD);
      
//    browser.setLayoutData( gridData );
    toolkit.adapt( browser );
      try
      {
        URL htmlURL = 
          KalypsoModel1D2DPlugin.getDefault().getBundle().getEntry( "/help/grid_widget_small_help.html" );
//        URL htmlURL = 
//          GridWidgetFace.class.getResource( "grid_widget_small_help.html" );
        browser.setUrl( FileLocator.toFileURL(  htmlURL ).toExternalForm());
        
        System.out.println("URI="+htmlURL.toURI().toASCIIString());
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      
      
    }
    
   
    private Image getImage(String path)
    {
      URL url = KalypsoModel1D2DPlugin.getDefault().getBundle().getEntry(path);
      ImageDescriptor desc = ImageDescriptor.createFromURL(url);
      Image createdImage = desc.createImage();
      
      return createdImage;
    }
    
    private ImageDescriptor getImageDescriptor(String path)
    {
      URL url = KalypsoModel1D2DPlugin.getDefault().getBundle().getEntry(path);
      ImageDescriptor desc = ImageDescriptor.createFromURL(url);
      
      return desc;
    }
    
    

  }