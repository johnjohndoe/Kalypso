package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;

public class FeatureNameLabelProvider extends GMLLabelProvider
{
  @Override
  public String getText( Object element )
  {
    if( element instanceof Feature )
    {
      final Object object = ((Feature) element).getProperty( new QName( NS.GML3, "name" ) );
      String result = null;
      if( object instanceof List )
      {
        if( ((List<Object>) object).size() > 0 )
          result = ((List<Object>) object).get( 0 ).toString();
      }
      else
        result = object.toString();
      if( result != null && result.trim().length() > 0 )
        return result;
      return "<name not specified>";
    }
    else
      return super.getText( element );
  }
}
