package org.kalypso.editor.tableeditor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.internal.resources.ResourceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.editor.mapeditor.WidgetAction;
import org.kalypso.xml.tableview.ObjectFactory;
import org.kalypso.xml.tableview.Tableview;
import org.kalypso.xml.types.TableviewLayerType;

/**
 * <p>
 * Eclipse-Editor zum editieren der GML-Gis-Templates.
 * </p>
 * 
 * <p>
 * Zeigt das ganze als Kartendarstellug, die einzelnen Datenquellen k?nnen
 * potentiell editiert werden
 * </p>
 * 
 * <p>
 * Implementiert {@link org.kalypso.util.command.ICommandManager}f?r die Undo
 * und Redo Action. Gibt alles an den DefaultCommandManager weiter, es wird
 * zus?tzlich eine Aktualisierung der View bei jeder Aktion durchgef?hrt
 * </p>
 * 
 * @author belger
 */
public class GisTableEditor extends AbstractEditorPart
{
  private final ObjectFactory m_tableviewObjectFactory = new ObjectFactory();

  private final Unmarshaller m_unmarshaller;

  private final Marshaller m_marshaller;

  private LayerTable m_layerTable = null;

  public GisTableEditor()
  {
    try
    {
      m_unmarshaller = m_tableviewObjectFactory.createUnmarshaller();
      m_marshaller = m_tableviewObjectFactory.createMarshaller();
    }
    catch( JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }
  
  protected FullAction[] createFullActions()
  {
    final List list = new ArrayList();

    return (FullAction[])list.toArray(new FullAction[list.size()]);
  }

  protected WidgetAction[] createWidgetActions()
  {
    final List list = new ArrayList();
    return (WidgetAction[])list.toArray( new WidgetAction[list.size()] );
  }

  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    if( m_layerTable == null )
      return;

    try
    {
      final TableviewLayerType layerType = m_layerTable.createLayerType();
      
      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      m_marshaller.marshal( layerType, bos );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );

      final IFile file = input.getFile();
      if( file.exists() )
        file.setContents( bis, false, true, monitor );
      else
        file.create( bis, false, monitor );

      bis.close();

      setDirty( false );
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    m_layerTable = new LayerTable( parent );

    load();
  
  }

  protected void load()
  {
      if(m_layerTable==null)
        return;
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    Tableview tableview = null;
    
    try
    {
      tableview = (Tableview)m_unmarshaller.unmarshal( input.getStorage().getContents() );
    }
    catch( final ResourceException re )
    {
      // TODO: handle ResourceException: e.g. Resource is out of sync
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }

    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();
    m_layerTable.setTableview( tableview, project );

    setDirty( false );

    setContentDescription( input.getFile().getName() );
    setPartName( input.getFile().getName() );
  }


}