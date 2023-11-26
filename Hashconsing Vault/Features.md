
## thread_safe

## auto_cleanup

```mermaid
erDiagram
       Combinations {
            default not_Thread_safe_and_auto_cleanup
            auto_cleanup_false not_thread_safe_and_not_auto_cleanup
            thread_safe
            VARCHAR(255) title
            TEXT description
            VARCHAR(255) url
            VARCHAR(255) thumbnail_url
            Timestamp taken_at
            Timestamp uploaded_at
            BOOLEAN is_public
            ForeignKey(album_id) Album
        }

```


## testing script

- single-threaded auto_cleanup enabled
- single-threaded auto_cleanup disabled
- thread-safe auto_cleanup enabled
- thread-safe auto_cleanup disabled