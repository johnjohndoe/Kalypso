package org.kalypso.ui.view.prognose;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.model.xml.ModellistType;
import org.kalypso.model.xml.ModellistType.ModelType;

/**
 * @author Belger
 */
public class ModelLabelProvider extends LabelProvider
{
  public String getText( final Object element )
  {
    final ModellistType.ModelType model = (ModelType)element;
    
    return model.getLabel();
  }
}