package de.tuhh.wb.javagis.data.event;

import java.util.ArrayList;
import java.util.List;

import de.tuhh.wb.javagis.data.TrippelKeyHash;
import ejb.event.EJBEvent;
public class KalypsoEventManager
{
    private static KalypsoEventManager instance=null;
    private List versionListeners;
    //fire: versionChangedEvent

    //    private Hashtable elementListeners; // sorted by Versions

    private TrippelKeyHash tableListeners; // sorted by themeKey/VersionId/elementTablesNr
    // register: GisElementClass
    // fire: createEvent,removeEvent,simplePropertyChangeEvent
    


    private List vectorSetListeners;
    private List simplePropertyListeners;
    
    private KalypsoEventManager()
    {
	this.versionListeners=new ArrayList();
	//	this.elementListeners=new Hashtable();
	this.tableListeners=new TrippelKeyHash();
	this.vectorSetListeners=new ArrayList();
    }

    public static KalypsoEventManager getInstance()
    {
	if(KalypsoEventManager.instance==null)
	    KalypsoEventManager.instance=new KalypsoEventManager();
	return KalypsoEventManager.instance;
    }

    public synchronized void addVersionListener(VersionListener listener)
    {
	if(!versionListeners.contains(listener))
	    versionListeners.add(listener);	
    }

    public synchronized void removeVersionListener(VersionListener listener)
    {
	versionListeners.remove(listener);
    }

    public synchronized void addTableListener(String themeKey,Object vId,int elementTable,TableListener listener)
    {
	Integer table=new Integer(elementTable);
	if(!tableListeners.containsKey(themeKey,vId,table))
	    {
		tableListeners.put(themeKey,vId,table,new ArrayList());
	    }
	List listeners=(List)tableListeners.get(themeKey,vId,table);
	if(!listeners.contains(listener))
	    listeners.add(listener);
    }

    public synchronized void removeTableListener(String themeKey,Object vId,int elementTable,TableListener listener)
    {
	Integer table=new Integer(elementTable);
	if(tableListeners.containsKey(themeKey,vId,table))
	    {
		List listeners=(List)tableListeners.get(themeKey,vId,table);
		listeners.remove(listener);
	    }
    }

    /*
      public synchronized void addElementListener(Object vId,ElementListener listener)
      {
      if(!elementListeners.containsKey(vId))
      elementListeners.put(vId,new ArrayList());
      List listeners=(List)elementListeners.get(vId);
      if(!listeners.contains(listener))
      listeners.add(listener);	
      }
      
      public synchronized void removeElementListener(ElementListener listener)
      {
      for(Enumeration enum=elementListeners.keys();enum.hasMoreElements();)
      {
      // remove in All
      List listeners=(List)elementListeners.get(enum.nextElement());
      listeners.remove(listener);
      }	  
      }
      
    */
    // EJBEventListener:
    public void notify(EJBEvent event)
    {
	System.out.println("got EVENT from DB "+event.getEventType());
	int elementTable=event.getElementTable();
	switch(event.getEventType())
	    {
	    case EJBEvent.OBJECT_CREATE:
	    case EJBEvent.RELATION_CREATE:
		//		fireElementCreateEvent(event);
		fireTableElementCreateEvent(event);
		System.out.println("object created");
		break;
	    case EJBEvent.SIMPLEPROPERTY_CHANGE:
		fireSimplePropertyChanged(event);
		break;
	    case EJBEvent.OBJECT_REMOVE:
	    case EJBEvent.RELATION_REMOVE:
		//		fireElementRemoveEvent(event);
		fireTableElementRemoveEvent(event);
		System.out.println("object removed");
		break;
	    case EJBEvent.VERSION_CREATE:
		fireVersionChangedEvent(event);
		System.out.println("version created");
		break;
	    case EJBEvent.VERSION_RENAME:
		fireVersionChangedEvent(event);
		System.out.println("version renamed");
		break;
	    case EJBEvent.VERSION_REMOVE:
		fireVersionChangedEvent(event);
		System.out.println("version removed");
		break;
	    default:
		break;
	    }
    }

    /*    private void fireElementCreateEvent(EJBEvent event)
    {
	Object vId=event.getVersionId();
	Object eId=event.getElementId();
	int elementTable=event.getElementTable();
	if(elementListeners.containsKey(vId))
	    {
		Iterator it=(new ArrayList((List)elementListeners.get(vId))).iterator();
		while(it.hasNext())
		    {
			((ElementListener)it.next()).onElementCreate(elementTable,eId);
		    }
	    }
    }

    private void fireElementRemoveEvent(EJBEvent event)
    {
	Object vId=event.getVersionId();
	Object eId=event.getElementId();
	int elementTable=event.getElementTable();
	if(elementListeners.containsKey(vId))
	    {
		Iterator it=(new ArrayList((List)elementListeners.get(vId))).iterator();
		while(it.hasNext())
		    {
			((ElementListener)it.next()).onElementRemove(elementTable,eId);
		    }
	    }
    }
    */
    private void fireTableElementCreateEvent(EJBEvent event)
    {
	Object vId=event.getVersionId();
	Object eId=event.getElementId();
	String themeKey=event.getThemeKey();
	Integer table=new Integer(event.getElementTable());
	if(tableListeners.containsKey(themeKey,vId,table))
	    {
		TableListener listener;
		List listeners=(List)tableListeners.get(themeKey,vId,table);
		for(int i = 0; i < listeners.size(); i++)
		    {
			listener = (TableListener)listeners.get(i);
			listener.onTableElementCreate(event.getElementTable(),eId);
		    }
	    }
    }

    private void fireTableElementRemoveEvent(EJBEvent event)
    {
	Object vId=event.getVersionId();
	Object eId=event.getElementId();
	String themeKey=event.getThemeKey();
	Integer table=new Integer(event.getElementTable());
	if(tableListeners.containsKey(themeKey,vId,table))
	    {
		TableListener listener;
		List listeners=(List)tableListeners.get(themeKey,vId,table);
		for(int i = 0; i < listeners.size(); i++)
		    {
			listener = (TableListener)listeners.get(i);
			listener.onTableElementRemove(event.getElementTable(),eId);
		    }
	    }
    }
    private void fireSimplePropertyChanged(EJBEvent event)
    {
	String themeKey=event.getThemeKey();
	Object vId=event.getVersionId();
	Object eId=event.getElementId();
	Integer table=new Integer(event.getElementTable());
	if(tableListeners.containsKey(themeKey,vId,table))
	    {
		TableListener listener;
		List listeners=(List)tableListeners.get(themeKey,vId,table);
		for(int i = 0; i < listeners.size(); i++)
		    {
			listener = (TableListener)listeners.get(i);
			listener.onSimplePropertyChanged(event.getElementTable(),eId);
		    }
	    }
}

    private void fireVersionChangedEvent(EJBEvent event)
    {
	VersionListener listener=null;
	for(int i = 0; i < versionListeners.size(); i++)
	    {
		listener = (VersionListener)versionListeners.get(i);
		listener.onVersionChanged(event);
	    }
    }
}

