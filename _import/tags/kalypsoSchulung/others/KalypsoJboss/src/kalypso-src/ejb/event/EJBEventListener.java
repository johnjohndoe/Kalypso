package ejb.event;

public interface EJBEventListener {

    public void notify(EJBEvent event);
    
}
