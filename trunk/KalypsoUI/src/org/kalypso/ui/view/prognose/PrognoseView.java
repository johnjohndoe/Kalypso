package org.kalypso.ui.view.prognose;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.internal.util.BundleUtility;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.eclipse.swt.graphics.FontUtilities;
import org.kalypso.model.xml.Modellist;
import org.kalypso.model.xml.ModellistType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.ModellistType.ModelType;
import org.kalypso.ui.ImageProvider;
import org.xml.sax.InputSource;

/**
 * @author belger
 */
public class PrognoseView extends ViewPart
{
  private Button m_button;

  private Modellist m_modellist = null;

  private final ModelLabelProvider m_labelProvider = new ModelLabelProvider();

  /** Alle Fonts in dieser Collection werden in der dispose Methode auch disposed */
  private Collection m_disposeFonts = new LinkedList();
  
  public PrognoseView()
  {
    try
    {
      final URL url = BundleUtility.find( "org.kalypso.ui", "etc/modellist.xml" );
      final InputSource inputSource = new InputSource( url.openStream() );
      m_modellist = (Modellist)new ObjectFactory().createUnmarshaller().unmarshal(
          inputSource );
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  public void dispose()
  {
    m_labelProvider.dispose();
    
    for( Iterator iter = m_disposeFonts.iterator(); iter.hasNext(); )
      ((Font)iter.next()).dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.intro.IIntroPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    final FormToolkit toolkit = new FormToolkit( parent.getDisplay() );
    final Form form = toolkit.createForm( parent );

    final GridLayout gridLayout = new GridLayout( 2, false );
    form.getBody().setLayout( gridLayout );
    final GridData formGridData = new GridData( GridData.FILL_BOTH );
    formGridData.horizontalAlignment = GridData.CENTER;
    form.setLayoutData( formGridData );
    
    final Label mainImageLabel = toolkit.createLabel( form.getBody(), null, SWT.NONE );
    final GridData mainLabelgridData = new GridData(  );
    mainLabelgridData.horizontalSpan = 2;
    mainLabelgridData.horizontalAlignment = GridData.CENTER;
    mainLabelgridData.verticalSpan = 1;
    mainLabelgridData.verticalAlignment = GridData.FILL;
    mainImageLabel.setLayoutData( mainLabelgridData );
    final Image createImage = ImageProvider.id( "etc/sachsenlogo.gif" ).createImage();
    mainImageLabel.setImage( createImage );

    final Label headingLabel = new Label( form.getBody(), SWT.SINGLE );
    final GridData headingGridData = new GridData( GridData.BEGINNING, GridData.BEGINNING, false, false, 2, 2 );
    headingLabel.setLayoutData( headingGridData );
    final Font headingFont = FontUtilities.createChangedFontData( headingLabel.getFont().getFontData(), 10, SWT.BOLD, headingLabel.getDisplay() );
    headingLabel.setFont(headingFont);
    m_disposeFonts.add( headingFont );
    headingLabel.setText( "Bitte wählen Sie ein Vorhersagegebiet" );
    
    final List list = new List( form.getBody(), SWT.SINGLE );
    final GridData listGridData = new GridData( GridData.BEGINNING, GridData.BEGINNING, false, false );
    list.setLayoutData( listGridData );
    list.setToolTipText( "Doppelklick startet die Prognoserechnung" );
    final Font listfont = FontUtilities.createChangedFontData( list.getFont().getFontData(), 10, SWT.NONE, list.getDisplay() ); 
    list.setFont( listfont );
    m_disposeFonts.add( listfont );
    
    final ListViewer viewer = new ListViewer( list );
    viewer.setContentProvider( new ArrayContentProvider( ) );
    viewer.setLabelProvider( m_labelProvider );
    
    final Label imageLabel = toolkit.createLabel( form.getBody(), null, SWT.NONE );
    imageLabel.setLayoutData( new GridData() ); 

    m_button = toolkit.createButton( form.getBody(), "Prognoserechnung starten", SWT.PUSH );
    m_button.setEnabled( false );

    // event handling
    viewer.addDoubleClickListener( new IDoubleClickListener()
    {
      public void doubleClick( DoubleClickEvent event )
      {
        startModel( event.getSelection() );
      }
    } );

    final Control button = m_button;
    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ModellistType.ModelType model = (ModelType)((IStructuredSelection)event.getSelection()).getFirstElement();
        
        button.setEnabled( model != null );
        
        final Image oldImage = imageLabel.getImage();
        
        final Image newImage = ImageProvider.id( model.getImage() ).createImage();
        
        imageLabel.setImage( newImage );
        
        if( oldImage != null )
          oldImage.dispose();
        
        form.getBody().layout();
        form.getBody().redraw();
      }
    } );

    m_button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        startModel( viewer.getSelection() );
      }
    } );

    // create content
    viewer.setInput( m_modellist.getModel() );
    viewer.setSelection( new StructuredSelection( m_modellist.getModel().get( 0 ) ) );
    parent.getDisplay().asyncExec( new Runnable( ) {
      public void run()
      {
        viewer.refresh();
      }} );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    // mir doch egal
  }

  protected void startModel( final ISelection selection )
  {
    // TODO: Stattdessen den Calcwizard starten
    
//    final IStructuredSelection sselection = (IStructuredSelection)selection;
    if( !selection.isEmpty() )
    {
//      final ModellistType.ModelType model = (ModelType)sselection.getFirstElement();
//      ModelNature.runPrognose( getViewSite().getShell(), model.getName() );
    }
  }}
