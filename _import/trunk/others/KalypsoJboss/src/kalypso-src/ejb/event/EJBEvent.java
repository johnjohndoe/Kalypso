package ejb.event;



public class EJBEvent implements java.io.Serializable {
    
    public static final int OBJECT_CREATE   = 1;
    public static final int OBJECT_REMOVE   = 2;
    public static final int OBJECT_BASEPOINT_CHANGE = 3;
    public static final int RELATION_CREATE = 4;
    public static final int RELATION_REMOVE = 5;
    public static final int RELATION_CHANGE = 6;

    public static final int SIMPLEPROPERTY_CHANGE = 7;
    public static final int VECTORSET_CHANGE = 8;
    public static final int VERSION_CREATE = 9;
    public static final int VERSION_REMOVE = 10;
    public static final int VERSION_RENAME = 11;

    public static final int VERSION_CLOSEVIEWS = 12;
    // this is for complex operations on versions such as importxml
    // where no events ar send to the client (to avoid thousands of events)
    // so its a good idea to close all views on this version
    // parameters: themeKey,vId

    private String myThemeKey;
    private int myEventType;
    private int myElementTable;
    private Object myVersionId;
    private Object myElementId;
    //    private Handle eventSource;

    public EJBEvent(String themeKey,int eventType,Object vId,int elementTable,Object eId)
    {
	this.myThemeKey=themeKey;
	this.myEventType=eventType;
	this.myVersionId=vId;
	this.myElementTable=elementTable;
	this.myElementId=eId;
    }
    
    /*
      public EJBEvent(int type, Handle source) 
      {
      eventSource = source;
      eventType   = type;
      }
      
      public Handle getEventSource() 
      {
      return eventSource;
      }
    */

    public String getThemeKey()
    {
	return myThemeKey;
    }
    public int getEventType() 
    {
        return myEventType;
    }
    public int getElementTable() 
    {
        return myElementTable;
    }
    public Object getVersionId() 
    {
        return myVersionId;
    }
    public Object getElementId() 
    {
        return myElementId;
    }
}
