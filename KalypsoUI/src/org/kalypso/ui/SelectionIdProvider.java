package org.kalypso.ui;

import java.util.HashMap;

import org.kalypso.ogc.gml.SelectionId;

/**
 * @author doemming
 */
public class SelectionIdProvider
{
  private int mySelections;
  private final HashMap mySelectionIds;
  public SelectionIdProvider()
  {
    mySelectionIds=new HashMap();
    mySelections=0;
  }
  
  public SelectionId createSelectionId(String name)
  {
    for(int id=1;id<Integer.MAX_VALUE;id*=2)
      if(id==(mySelections&id))
      {
        mySelections|=id;
                return new SelectionId(name,id);
      } 
      System.out.println("no more selections");
    return null;
  }
  
  public void releaseSelectionId(SelectionId selectionId)
  {
    mySelections^=selectionId.getId();
    mySelectionIds.remove(selectionId);    
  }  
  
  public SelectionId getSelectionId(String name)
  {
       return (SelectionId)mySelectionIds.get(name);
  }
  
  public SelectionId[] getAllSelectionIds()
  {
      return (SelectionId[])mySelectionIds.keySet().toArray(new SelectionId[mySelectionIds.size()]);
  }
}
