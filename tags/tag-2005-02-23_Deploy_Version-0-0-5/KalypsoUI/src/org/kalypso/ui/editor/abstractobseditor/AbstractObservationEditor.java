package org.kalypso.ui.editor.abstractobseditor;

import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.ogc.sensor.template.AbstractViewTemplate;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * AbstractObsEditor
 * 
 * @author schlienger
 */
public abstract class AbstractObservationEditor extends AbstractEditorPart
{
  private AbstractViewTemplate m_template;

  private ObservationEditorOutlinePage m_outline = null;

  public AbstractObservationEditor( AbstractViewTemplate template )
  {
    m_template = template;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    if( m_template != null )
      m_template.dispose();

    if( m_outline != null )
      m_outline.dispose();
    
    super.dispose();
  }
  
  /**
   * @return template
   */
  public AbstractViewTemplate getTemplate( )
  {
    return m_template;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    if( adapter == IContentOutlinePage.class )
    {
      // lazy loading
      if( m_outline == null || m_outline.getControl() != null
          && m_outline.getControl().isDisposed() )
      {
        // dispose when not null (not sure if this is ok)
        if( m_outline != null )
          m_outline.dispose();

        m_outline = new ObservationEditorOutlinePage( this );
        m_outline.setTemplate( m_template );
      }

      return m_outline;
    }
    return null;
  }
}