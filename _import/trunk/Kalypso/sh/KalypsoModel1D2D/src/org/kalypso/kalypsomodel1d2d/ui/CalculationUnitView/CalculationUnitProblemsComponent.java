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
package org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.editor.ListLabelProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;

/**
 * @author Madanagopal
 *
 */
public class CalculationUnitProblemsComponent
{
  private FormToolkit toolkit;
  private Composite parent;
  private CalculationUnitDataModel dataModel;
  private KeyBasedDataModelChangeListener settingsKeyListener = new KeyBasedDataModelChangeListener(){
    public void dataChanged( final String key, final Object newValue )
    {
      Display display = parent.getDisplay();
      final Runnable runnable = new Runnable()
      {
        public void run( )
        {
          if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) ){
            if (newValue != null){
            updateThisSection( newValue );
            }            
          }
        }

      };
      display.syncExec( runnable );      
    }    
  };
  
  private Composite rootComposite;

  public void createControl( CalculationUnitDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
	    this.toolkit = toolkit;
	    this.parent = parent;
	    this.dataModel = dataModel;
	    guiProblemViewer( parent );
	    dataModel.addKeyBasedDataChangeListener( settingsKeyListener );
    
  }

  private void guiProblemViewer( Composite composite )
  {
	    rootComposite = new Composite(composite,SWT.FLAT);
	    rootComposite.setLayout( new FormLayout());
	    
	    FormData formData = new FormData();
	    formData.top = new FormAttachment(0,0);
	    formData.left = new FormAttachment(0,0);
	    rootComposite.setLayoutData( formData );
	    
	    Label nameText = new Label(rootComposite, SWT.NONE);
	    nameText.setText( "Problems :" );
	    
	    formData = new FormData();
	    formData.left = new FormAttachment(0,5);
	    formData.top = new FormAttachment(0,5);
	    nameText.setLayoutData( formData );
	    
	    Button refreshButton = new Button(rootComposite, SWT.NONE);
	    Image refreshImage = new Image( rootComposite.getDisplay(),
	            KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
	                    PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
	                    "icons/elcl16/refresh.gif" ).getImageData() );;
        refreshButton.setImage( refreshImage );
        
        refreshButton.addSelectionListener( new SelectionAdapter(){
          @Override
          public void widgetSelected( SelectionEvent e )
          {
            
          }
          
        });
        
        formData = new FormData();
        formData.left = new FormAttachment(nameText,10);
        formData.top = new FormAttachment(0,5);
        refreshButton.setLayoutData( formData );
        
        TableViewer problemTableViewer = new TableViewer( rootComposite, SWT.FILL | SWT.BORDER );
        Table problemsTable = problemTableViewer.getTable();
        problemTableViewer.setLabelProvider( new ListLabelProvider());
        problemTableViewer.setContentProvider( new ArrayContentProvider() );
        problemsTable.setLinesVisible( true );
        problemsTable.setLayoutData( formData ); 
        
        formData = new FormData();
        formData.left = new FormAttachment(0,5);
        formData.top = new FormAttachment(refreshButton,5);
        formData.right = new FormAttachment(100,-5);
        problemsTable.setLayoutData( formData );
       
  }
  private void updateThisSection( Object newValue )
  {
   //@TODO  
  }
}
